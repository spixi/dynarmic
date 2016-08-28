/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include <algorithm>
#include <array>
#include <cinttypes>
#include <cstdio>
#include <cstring>
#include <functional>
#include <tuple>
#include <vector>

#include <catch.hpp>

#include <dynarmic/dynarmic.h>

#include "common/bit_util.h"
#include "common/common_types.h"
#include "frontend/arm_types.h"
#include "frontend/disassembler/disassembler.h"
#include "frontend/ir/basic_block.h"
#include "frontend/translate/translate.h"
#include "ir_opt/passes.h"
#include "rand_int.h"
#include "skyeye_interpreter/dyncom/arm_dyncom_interpreter.h"
#include "skyeye_interpreter/skyeye_common/armstate.h"
#include "valid_arm.h"

#ifdef __unix__
#include <signal.h>
#endif

using Dynarmic::Common::Bits;

struct WriteRecord {
    size_t size;
    u32 address;
    u64 data;
};

static bool operator==(const WriteRecord& a, const WriteRecord& b) {
    return std::tie(a.size, a.address, a.data) == std::tie(b.size, b.address, b.data);
}

static std::array<u32, 3000> code_mem{};
static std::vector<WriteRecord> write_records;

static bool IsReadOnlyMemory(u32 vaddr);
static u8 MemoryRead8(u32 vaddr);
static u16 MemoryRead16(u32 vaddr);
static u32 MemoryRead32(u32 vaddr);
static u64 MemoryRead64(u32 vaddr);
static void MemoryWrite8(u32 vaddr, u8 value);
static void MemoryWrite16(u32 vaddr, u16 value);
static void MemoryWrite32(u32 vaddr, u32 value);
static void MemoryWrite64(u32 vaddr, u64 value);
static void InterpreterFallback(u32 pc, Dynarmic::Jit* jit);
static Dynarmic::UserCallbacks GetUserCallbacks();

static bool IsReadOnlyMemory(u32 vaddr) {
    return vaddr < code_mem.size();
}
static u8 MemoryRead8(u32 vaddr) {
    return static_cast<u8>(vaddr);
}
static u16 MemoryRead16(u32 vaddr) {
    return static_cast<u16>(vaddr);
}
static u32 MemoryRead32(u32 vaddr) {
    if (vaddr < code_mem.size() * sizeof(u32)) {
        size_t index = vaddr / sizeof(u32);
        return code_mem[index];
    }
    return vaddr;
}
static u64 MemoryRead64(u32 vaddr) {
    return MemoryRead32(vaddr) | (u64(MemoryRead32(vaddr+4)) << 32);
}

static void MemoryWrite8(u32 vaddr, u8 value){
    write_records.push_back({8, vaddr, value});
}
static void MemoryWrite16(u32 vaddr, u16 value){
    write_records.push_back({16, vaddr, value});
}
static void MemoryWrite32(u32 vaddr, u32 value){
    write_records.push_back({32, vaddr, value});
}
static void MemoryWrite64(u32 vaddr, u64 value){
    write_records.push_back({64, vaddr, value});
}

static void InterpreterFallback(u32 pc, Dynarmic::Jit* jit) {
    ARMul_State interp_state{USER32MODE};
    interp_state.user_callbacks = GetUserCallbacks();
    interp_state.NumInstrsToExecute = 1;

    interp_state.Reg = jit->Regs();
    interp_state.ExtReg = jit->ExtRegs();
    interp_state.Cpsr = jit->Cpsr();
    interp_state.VFP[VFP_FPSCR] = jit->Fpscr();
    interp_state.Reg[15] = pc;

    InterpreterClearCache();
    InterpreterMainLoop(&interp_state);

    bool T = Dynarmic::Common::Bit<5>(interp_state.Cpsr);
    interp_state.Reg[15] &= T ? 0xFFFFFFFE : 0xFFFFFFFC;

    jit->Regs() = interp_state.Reg;
    jit->ExtRegs() = interp_state.ExtReg;
    jit->Cpsr() = interp_state.Cpsr;
    jit->SetFpscr(interp_state.VFP[VFP_FPSCR]);
}

static void Fail() {
    FAIL();
}

static Dynarmic::UserCallbacks GetUserCallbacks() {
    Dynarmic::UserCallbacks user_callbacks{};
    user_callbacks.InterpreterFallback = &InterpreterFallback;
    user_callbacks.CallSVC = (bool (*)(u32)) &Fail;
    user_callbacks.IsReadOnlyMemory = &IsReadOnlyMemory;
    user_callbacks.MemoryRead8 = &MemoryRead8;
    user_callbacks.MemoryRead16 = &MemoryRead16;
    user_callbacks.MemoryRead32 = &MemoryRead32;
    user_callbacks.MemoryRead64 = &MemoryRead64;
    user_callbacks.MemoryWrite8 = &MemoryWrite8;
    user_callbacks.MemoryWrite16 = &MemoryWrite16;
    user_callbacks.MemoryWrite32 = &MemoryWrite32;
    user_callbacks.MemoryWrite64 = &MemoryWrite64;
    return user_callbacks;
}

struct InstructionGenerator final {
public:
    InstructionGenerator(const char* format, std::function<bool(u32)> is_valid = [](u32){ return true; }) : is_valid(is_valid) {
        REQUIRE(strlen(format) == 32);

        for (int i = 0; i < 32; i++) {
            const u32 bit = 1u << (31 - i);
            switch (format[i]) {
            case '0':
                mask |= bit;
                break;
            case '1':
                bits |= bit;
                mask |= bit;
                break;
            default:
                // Do nothing
                break;
            }
        }
    }
    u32 Generate(bool condition = true) const {
        u32 inst;
        do {
            u32 random = RandInt<u32>(0, 0xFFFFFFFF);
            if (condition)
                random &= ~(0xF << 28);
            inst = bits | (random & ~mask);
        } while (!is_valid(inst));

        if (condition) {
            // Have a one-in-twenty-five chance of actually having a cond.
            if (RandInt(1, 25) == 1)
                inst |= RandInt(0x0, 0xD) << 28;
            else
                inst |= 0xE << 28;
        }

        return inst;
    }
    u32 Bits() const { return bits; }
    u32 Mask() const { return mask; }
    bool IsValid(u32 inst) const { return is_valid(inst); }
private:
    u32 bits = 0;
    u32 mask = 0;
    std::function<bool(u32)> is_valid;
};

static bool DoesBehaviorMatch(const ARMul_State& interp, const Dynarmic::Jit& jit, const std::vector<WriteRecord>& interp_write_records, const std::vector<WriteRecord>& jit_write_records) {
    return interp.Reg == jit.Regs()
           && interp.ExtReg == jit.ExtRegs()
           && interp.Cpsr == jit.Cpsr()
           && interp.VFP[VFP_FPSCR] == jit.Fpscr()
           && interp_write_records == jit_write_records;
}

void FuzzJitArm(const size_t instruction_count, const size_t instructions_to_execute_count, const size_t run_count, const std::function<u32()> instruction_generator) {
    // Prepare memory
    code_mem.fill(0xEAFFFFFE); // b +#0

    // Prepare test subjects
    ARMul_State interp{USER32MODE};
    interp.user_callbacks = GetUserCallbacks();
    Dynarmic::Jit jit{GetUserCallbacks()};

    for (size_t run_number = 0; run_number < run_count; run_number++) {
        interp.instruction_cache.clear();
        InterpreterClearCache();
        jit.ClearCache(false);

        // Setup initial state

        u32 initial_cpsr = 0x000001D0;

        std::array<u32, 16> initial_regs;
        std::generate_n(initial_regs.begin(), 15, []{ return RandInt<u32>(0, 0xFFFFFFFF); });
        initial_regs[15] = 0;

        std::array<u32, 64> initial_extregs;
        std::generate_n(initial_extregs.begin(), 64, []{ return RandInt<u32>(0, 0xFFFFFFFF); });

        u32 initial_fpscr = 0x01000000 | (RandInt<u32>(0, 3) << 22);

        interp.UnsetExclusiveMemoryAddress();
        interp.Cpsr = initial_cpsr;
        interp.Reg = initial_regs;
        interp.ExtReg = initial_extregs;
        interp.VFP[VFP_FPSCR] = initial_fpscr;
        jit.Reset();
        jit.Cpsr() = initial_cpsr;
        jit.Regs() = initial_regs;
        jit.ExtRegs() = initial_extregs;
        jit.SetFpscr(initial_fpscr);

        std::generate_n(code_mem.begin(), instruction_count, instruction_generator);

        // Run interpreter
        write_records.clear();
        interp.NumInstrsToExecute = static_cast<unsigned>(instructions_to_execute_count);
        InterpreterMainLoop(&interp);
        auto interp_write_records = write_records;
        {
            bool T = Dynarmic::Common::Bit<5>(interp.Cpsr);
            interp.Reg[15] &= T ? 0xFFFFFFFE : 0xFFFFFFFC;
        }

        // Run jit
        write_records.clear();
        jit.Run(static_cast<unsigned>(instructions_to_execute_count));
        auto jit_write_records = write_records;

        // Compare
        if (!DoesBehaviorMatch(interp, jit, interp_write_records, jit_write_records)) {
            printf("Failed at execution number %zu\n", run_number);

            printf("\nInstruction Listing: \n");
            for (size_t i = 0; i < instruction_count; i++) {
                 printf("%x: %s\n", code_mem[i], Dynarmic::Arm::DisassembleArm(code_mem[i]).c_str());
            }

            printf("\nInitial Register Listing: \n");
            for (int i = 0; i <= 15; i++) {
                auto reg = Dynarmic::Arm::RegToString(static_cast<Dynarmic::Arm::Reg>(i));
                printf("%4s: %08x\n", reg, initial_regs[i]);
            }
            printf("CPSR: %08x\n", initial_cpsr);
            printf("FPSCR:%08x\n", initial_fpscr);
            for (int i = 0; i <= 63; i++) {
                printf("S%3i: %08x\n", i, initial_extregs[i]);
            }

            printf("\nFinal Register Listing: \n");
            printf("      interp   jit\n");
            for (int i = 0; i <= 15; i++) {
                auto reg = Dynarmic::Arm::RegToString(static_cast<Dynarmic::Arm::Reg>(i));
                printf("%4s: %08x %08x %s\n", reg, interp.Reg[i], jit.Regs()[i], interp.Reg[i] != jit.Regs()[i] ? "*" : "");
            }
            printf("CPSR: %08x %08x %s\n", interp.Cpsr, jit.Cpsr(), interp.Cpsr != jit.Cpsr() ? "*" : "");
            printf("FPSCR:%08x %08x %s\n", interp.VFP[VFP_FPSCR], jit.Fpscr(), interp.VFP[VFP_FPSCR] != jit.Fpscr() ? "*" : "");
            for (int i = 0; i <= 63; i++) {
                printf("S%3i: %08x %08x %s\n", i, interp.ExtReg[i], jit.ExtRegs()[i], interp.ExtReg[i] != jit.ExtRegs()[i] ? "*" : "");
            }

            printf("\nInterp Write Records:\n");
            for (auto& record : interp_write_records) {
                printf("%zu [%x] = %" PRIx64 "\n", record.size, record.address, record.data);
            }

            printf("\nJIT Write Records:\n");
            for (auto& record : jit_write_records) {
                printf("%zu [%x] = %" PRIx64 "\n", record.size, record.address, record.data);
            }

            size_t num_insts = 0;
            while (num_insts < instructions_to_execute_count) {
                Dynarmic::Arm::LocationDescriptor descriptor = {u32(num_insts * 4), false, false, 0};
                Dynarmic::IR::Block ir_block = Dynarmic::Arm::Translate(descriptor, &MemoryRead32);
                Dynarmic::Optimization::GetSetElimination(ir_block);
                Dynarmic::Optimization::DeadCodeElimination(ir_block);
                Dynarmic::Optimization::VerificationPass(ir_block);
                printf("\n\nIR:\n%s", Dynarmic::IR::DumpBlock(ir_block).c_str());
                printf("\n\nx86_64:\n%s", jit.Disassemble(descriptor).c_str());
                num_insts += ir_block.CycleCount();
            }

#ifdef _MSC_VER
            __debugbreak();
#endif
#ifdef __unix__
            raise(SIGTRAP);
#endif
            FAIL();
        }

        if (run_number % 10 == 0) printf("%zu\r", run_number);
    }
}

TEST_CASE( "arm: Test all arm instructions", "[arm]" ) {
    auto gen = []() -> u32 {
        return GenerateInstruction(false);
    };

    FuzzJitArm(1, 2, 1000000, gen);
    FuzzJitArm(5, 6, 1000000, gen);
    FuzzJitArm(32, 33, 1000000, gen);
    FuzzJitArm(1024, 1025, 1000000, gen);
}

TEST_CASE( "arm: Optimization Failure (Randomized test case)", "[arm]" ) {
    // This was a randomized test-case that was failing.
    //
    // IR produced for location {12, !T, !E} was:
    // %0     = GetRegister r1
    // %1     = SubWithCarry %0, #0x3e80000, #1
    // %2     = GetCarryFromOp %1
    // %3     = GetOverflowFromOp %1
    // %4     = MostSignificantBit %1
    //          SetNFlag %4
    // %6     = IsZero %1
    //          SetZFlag %6
    //          SetCFlag %2
    //          SetVFlag %3
    // %10    = GetRegister r5
    // %11    = AddWithCarry %10, #0x8a00, %2
    //          SetRegister r4, %11
    //
    // The reference to %2 in instruction %11 was the issue, because instruction %8
    // told the register allocator it was a Use but then modified the value.
    // Changing the EmitSet*Flag instruction to declare their arguments as UseScratch
    // solved this bug.

    Dynarmic::Jit jit{GetUserCallbacks()};
    code_mem.fill({});
    code_mem[0] = 0xe35f0cd9; // cmp pc, #55552
    code_mem[1] = 0xe11c0474; // tst r12, r4, ror r4
    code_mem[2] = 0xe1a006a7; // mov r0, r7, lsr #13
    code_mem[3] = 0xe35107fa; // cmp r1, #0x3E80000
    code_mem[4] = 0xe2a54c8a; // adc r4, r5, #35328
    code_mem[5] = 0xeafffffe; // b +#0

    jit.Regs() = {
            0x6973b6bb, 0x267ea626, 0x69debf49, 0x8f976895, 0x4ecd2d0d, 0xcf89b8c7, 0xb6713f85, 0x15e2aa5,
            0xcd14336a, 0xafca0f3e, 0xace2efd9, 0x68fb82cd, 0x775447c0, 0xc9e1f8cd, 0xebe0e626, 0x0
    };
    jit.Cpsr() = 0x000001d0; // User-mode

    jit.Run(6);

    REQUIRE( jit.Regs()[0] == 0x00000af1 );
    REQUIRE( jit.Regs()[1] == 0x267ea626 );
    REQUIRE( jit.Regs()[2] == 0x69debf49 );
    REQUIRE( jit.Regs()[3] == 0x8f976895 );
    REQUIRE( jit.Regs()[4] == 0xcf8a42c8 );
    REQUIRE( jit.Regs()[5] == 0xcf89b8c7 );
    REQUIRE( jit.Regs()[6] == 0xb6713f85 );
    REQUIRE( jit.Regs()[7] == 0x015e2aa5 );
    REQUIRE( jit.Regs()[8] == 0xcd14336a );
    REQUIRE( jit.Regs()[9] == 0xafca0f3e );
    REQUIRE( jit.Regs()[10] == 0xace2efd9 );
    REQUIRE( jit.Regs()[11] == 0x68fb82cd );
    REQUIRE( jit.Regs()[12] == 0x775447c0 );
    REQUIRE( jit.Regs()[13] == 0xc9e1f8cd );
    REQUIRE( jit.Regs()[14] == 0xebe0e626 );
    REQUIRE( jit.Regs()[15] == 0x00000014 );
    REQUIRE( jit.Cpsr() == 0x200001d0 );
}

TEST_CASE( "arm: SMUAD corner case", "[JitX64]" ) {
    // This is a corner case that is fairly unlikely to be tested in our
    // randomised test-cases, hence we test it manually here.

    Dynarmic::Jit jit{GetUserCallbacks()};
    code_mem.fill({});
    code_mem[0] = 0xe700f211; // smuad r0, r1, r2
    code_mem[1] = 0xeafffffe; // b +#0

    jit.Regs()[0] = 0;
    jit.Regs()[1] = 0x80008000;
    jit.Regs()[2] = 0x80008000;
    jit.Cpsr() = 0x000001d0; // User-mode

    jit.Run(1);

    REQUIRE( jit.Regs()[0] == 0x80000000 );
    REQUIRE( jit.Regs()[1] == 0x80008000 );
    REQUIRE( jit.Regs()[2] == 0x80008000 );
    REQUIRE( jit.Regs()[15] == 4 );
    REQUIRE( jit.Cpsr() == 0x080001d0 ); // Q flag, User-mode
}

struct VfpTest {
    u32 initial_fpscr;
    u32 a;
    u32 b;
    u32 result;
    u32 final_fpscr;
};

TEST_CASE("vfp: vadd", "[vfp]") {
    Dynarmic::Jit jit{GetUserCallbacks()};
    code_mem.fill({});
    code_mem[0] = 0xee323a01; // vadd.f32 s6, s4, s2
    code_mem[1] = 0xeafffffe; // b +#0

    std::vector<VfpTest> tests {
#include "vadd.vfp_tests.inc"
    };

    for (const auto& test : tests) {
        jit.Regs()[15] = 0;
        jit.Cpsr() = 0x000001d0;
        jit.ExtRegs()[4] = test.a;
        jit.ExtRegs()[2] = test.b;
        jit.SetFpscr(test.initial_fpscr);

        jit.Run(2);

        REQUIRE( jit.Regs()[15] == 4 );
        REQUIRE( jit.Cpsr() == 0x000001d0 );
        REQUIRE( jit.ExtRegs()[6] == test.result );
        REQUIRE( jit.ExtRegs()[4] == test.a );
        REQUIRE( jit.ExtRegs()[2] == test.b );
        REQUIRE( jit.Fpscr() == test.final_fpscr );
    }
}

TEST_CASE("VFP: VMOV", "[JitX64][vfp]") {
    const auto is_valid = [](u32 instr) -> bool {
        return Bits<0, 6>(instr) != 0b111111
                && Bits<12, 15>(instr) != 0b1111
                && Bits<16, 19>(instr) != 0b1111
                && Bits<12, 15>(instr) != Bits<16, 19>(instr);
    };

    const std::array<InstructionGenerator, 8> instructions = {{
        InstructionGenerator("cccc11100000ddddtttt1011D0010000", is_valid),
        InstructionGenerator("cccc11100001nnnntttt1011N0010000", is_valid),
        InstructionGenerator("cccc11100000nnnntttt1010N0010000", is_valid),
        InstructionGenerator("cccc11100001nnnntttt1010N0010000", is_valid),
        InstructionGenerator("cccc11000100uuuutttt101000M1mmmm", is_valid),
        InstructionGenerator("cccc11000101uuuutttt101000M1mmmm", is_valid),
        InstructionGenerator("cccc11000100uuuutttt101100M1mmmm", is_valid),
        InstructionGenerator("cccc11000101uuuutttt101100M1mmmm", is_valid),
    }};

    FuzzJitArm(1, 1, 10000, [&instructions]() -> u32 {
        return instructions[RandInt<size_t>(0, instructions.size() - 1)].Generate();
    });
}

TEST_CASE("VFP: VMOV (reg), VLDR, VSTR", "[JitX64][vfp]") {
    const std::array<InstructionGenerator, 4> instructions = {{
        InstructionGenerator("1111000100000001000000e000000000"), // SETEND
        InstructionGenerator("cccc11101D110000dddd101z01M0mmmm"), // VMOV (reg)
        InstructionGenerator("cccc1101UD01nnnndddd101zvvvvvvvv"), // VLDR
        InstructionGenerator("cccc1101UD00nnnndddd101zvvvvvvvv"), // VSTR
    }};

    FuzzJitArm(5, 6, 10000, [&instructions]() -> u32 {
        return instructions[RandInt<size_t>(0, instructions.size() - 1)].Generate();
    });
}

TEST_CASE("VFP: VPUSH, VPOP", "[JitX64][vfp]") {
    const auto is_valid = [](u32 instr) -> bool {
        auto regs = (instr & 0x100) ? (Bits<0, 7>(instr) >> 1) : Bits<0, 7>(instr);
        auto base = Bits<12, 15>(instr);
        unsigned d;
        if (instr & 0x100) {
            d = (base + ((instr & 0x400000) ? 16 : 0));
        } else {
            d = ((base << 1) + ((instr & 0x400000) ? 1 : 0));
        }
        // if regs == 0 || regs > 16 || (d+regs) > 32 then UNPREDICTABLE
        return regs != 0 && regs <= 16 && (d + regs) <= 32;
    };

    const std::array<InstructionGenerator, 2> instructions = {{
        InstructionGenerator("cccc11010D101101dddd101zvvvvvvvv", is_valid), // VPUSH
        InstructionGenerator("cccc11001D111101dddd1010vvvvvvvv", is_valid), // VPOP
    }};

    FuzzJitArm(5, 6, 10000, [&instructions]() -> u32 {
        return instructions[RandInt<size_t>(0, instructions.size() - 1)].Generate();
    });
}
