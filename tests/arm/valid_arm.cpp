/* This file is part of the dynarmic project.
 * Copyright (c) 2016 MerryMage
 * This software may be used and distributed according to the terms of the GNU
 * General Public License version 2 or any later version.
 */

#include "common/bit_util.h"
#include "frontend/arm_types.h"
#include "frontend/decoder/arm.h"
#include "rand_int.h"

enum class Validity {
    Valid,
    ModifiesPC,
    Unpredictable,
    Unimplemented,
    TODO,
};

using namespace Dynarmic;
using namespace Dynarmic::Arm;

struct ValidArmVisitor final {
    // Branch instructions
    Validity arm_B(Cond cond, Imm24 imm24) {
        return Validity::ModifiesPC;
    }
    Validity arm_BL(Cond cond, Imm24 imm24) {
        return Validity::ModifiesPC;
    }
    Validity arm_BLX_imm(bool H, Imm24 imm24) {
        return Validity::ModifiesPC;
    }
    Validity arm_BLX_reg(Cond cond, Reg m) {
        return m == Reg::PC ? Validity::Unpredictable : Validity::ModifiesPC;
    }
    Validity arm_BX(Cond cond, Reg m) {
        return Validity::ModifiesPC;
    }
    Validity arm_BXJ(Cond cond, Reg m) {
        return Validity::ModifiesPC;
    }

    // Coprocessor instructions
    Validity arm_CDP() {
        return Validity::Unimplemented;
    }
    Validity arm_LDC() {
        return Validity::Unimplemented;
    }
    Validity arm_MCR() {
        return Validity::Unimplemented;
    }
    Validity arm_MCRR() {
        return Validity::Unimplemented;
    }
    Validity arm_MRC() {
        return Validity::Unimplemented;
    }
    Validity arm_MRRC() {
        return Validity::Unimplemented;
    }
    Validity arm_STC() {
        return Validity::Unimplemented;
    }

    // Data processing instructions
    Validity arm_ADC_imm(Cond cond, bool S, Reg n, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_ADC_reg(Cond cond, bool S, Reg n, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_ADC_rsr(Cond cond, bool S, Reg n, Reg d, Reg s, ShiftType shift, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC || s == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_ADD_imm(Cond cond, bool S, Reg n, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_ADD_reg(Cond cond, bool S, Reg n, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_ADD_rsr(Cond cond, bool S, Reg n, Reg d, Reg s, ShiftType shift, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC || s == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_AND_imm(Cond cond, bool S, Reg n, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_AND_reg(Cond cond, bool S, Reg n, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_AND_rsr(Cond cond, bool S, Reg n, Reg d, Reg s, ShiftType shift, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC || s == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_BIC_imm(Cond cond, bool S, Reg n, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_BIC_reg(Cond cond, bool S, Reg n, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_BIC_rsr(Cond cond, bool S, Reg n, Reg d, Reg s, ShiftType shift, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC || s == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_CMN_imm(Cond cond, Reg n, int rotate, Imm8 imm8) {
        return Validity::Valid;
    }
    Validity arm_CMN_reg(Cond cond, Reg n, Imm5 imm5, ShiftType shift, Reg m) {
        return Validity::Valid;
    }
    Validity arm_CMN_rsr(Cond cond, Reg n, Reg s, ShiftType shift, Reg m) {
        return n == Reg::PC || m == Reg::PC || s == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_CMP_imm(Cond cond, Reg n, int rotate, Imm8 imm8) {
        return Validity::Valid;
    }
    Validity arm_CMP_reg(Cond cond, Reg n, Imm5 imm5, ShiftType shift, Reg m) {
        return Validity::Valid;
    }
    Validity arm_CMP_rsr(Cond cond, Reg n, Reg s, ShiftType shift, Reg m) {
        return n == Reg::PC || m == Reg::PC || s == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_EOR_imm(Cond cond, bool S, Reg n, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_EOR_reg(Cond cond, bool S, Reg n, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_EOR_rsr(Cond cond, bool S, Reg n, Reg d, Reg s, ShiftType shift, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC || s == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_MOV_imm(Cond cond, bool S, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_MOV_reg(Cond cond, bool S, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_MOV_rsr(Cond cond, bool S, Reg d, Reg s, ShiftType shift, Reg m) {
        return d == Reg::PC || m == Reg::PC || s == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_MVN_imm(Cond cond, bool S, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_MVN_reg(Cond cond, bool S, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_MVN_rsr(Cond cond, bool S, Reg d, Reg s, ShiftType shift, Reg m) {
        return d == Reg::PC || m == Reg::PC || s == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_ORR_imm(Cond cond, bool S, Reg n, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_ORR_reg(Cond cond, bool S, Reg n, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_ORR_rsr(Cond cond, bool S, Reg n, Reg d, Reg s, ShiftType shift, Reg m) {
        return n == Reg::PC || m == Reg::PC || s == Reg::PC || d == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_RSB_imm(Cond cond, bool S, Reg n, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_RSB_reg(Cond cond, bool S, Reg n, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_RSB_rsr(Cond cond, bool S, Reg n, Reg d, Reg s, ShiftType shift, Reg m) {
        return n == Reg::PC || m == Reg::PC || s == Reg::PC || d == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_RSC_imm(Cond cond, bool S, Reg n, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_RSC_reg(Cond cond, bool S, Reg n, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_RSC_rsr(Cond cond, bool S, Reg n, Reg d, Reg s, ShiftType shift, Reg m) {
        return n == Reg::PC || m == Reg::PC || s == Reg::PC || d == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SBC_imm(Cond cond, bool S, Reg n, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_SBC_reg(Cond cond, bool S, Reg n, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_SBC_rsr(Cond cond, bool S, Reg n, Reg d, Reg s, ShiftType shift, Reg m) {
        return n == Reg::PC || m == Reg::PC || s == Reg::PC || d == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SUB_imm(Cond cond, bool S, Reg n, Reg d, int rotate, Imm8 imm8) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_SUB_reg(Cond cond, bool S, Reg n, Reg d, Imm5 imm5, ShiftType shift, Reg m) {
        return d == Reg::PC ? (S ? Validity::Unimplemented : Validity::ModifiesPC) : Validity::Valid;
    }
    Validity arm_SUB_rsr(Cond cond, bool S, Reg n, Reg d, Reg s, ShiftType shift, Reg m) {
        return n == Reg::PC || m == Reg::PC || s == Reg::PC || d == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_TEQ_imm(Cond cond, Reg n, int rotate, Imm8 imm8) {
        return Validity::Valid;
    }
    Validity arm_TEQ_reg(Cond cond, Reg n, Imm5 imm5, ShiftType shift, Reg m) {
        return Validity::Valid;
    }
    Validity arm_TEQ_rsr(Cond cond, Reg n, Reg s, ShiftType shift, Reg m) {
        return n == Reg::PC || m == Reg::PC || s == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_TST_imm(Cond cond, Reg n, int rotate, Imm8 imm8) {
        return Validity::Valid;
    }
    Validity arm_TST_reg(Cond cond, Reg n, Imm5 imm5, ShiftType shift, Reg m) {
        return Validity::Valid;
    }
    Validity arm_TST_rsr(Cond cond, Reg n, Reg s, ShiftType shift, Reg m) {
        return n == Reg::PC || m == Reg::PC || s == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }

    // Exception generating instructions
    Validity arm_BKPT(Cond cond, Imm12 imm12, Imm4 imm4) {
        return Validity::Unimplemented;
    }
    Validity arm_SVC(Cond cond, Imm24 imm24) {
        return Validity::Unimplemented;
    }
    Validity arm_UDF() {
        return Validity::Unimplemented;
    }

    // Extension instructions
    Validity arm_SXTAB(Cond cond, Reg n, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SXTAB16(Cond cond, Reg n, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SXTAH(Cond cond, Reg n, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SXTB(Cond cond, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SXTB16(Cond cond, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SXTH(Cond cond, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UXTAB(Cond cond, Reg n, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UXTAB16(Cond cond, Reg n, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UXTAH(Cond cond, Reg n, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UXTB(Cond cond, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UXTB16(Cond cond, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UXTH(Cond cond, Reg d, SignExtendRotation rotate, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }

    // Hint instructions
    Validity arm_PLD() {
        return Validity::Valid;
    }
    Validity arm_SEV() {
        return Validity::Valid;
    }
    Validity arm_WFE() {
        return Validity::Valid;
    }
    Validity arm_WFI() {
        return Validity::Valid;
    }
    Validity arm_YIELD() {
        return Validity::Valid;
    }

    // Load/Store instructions
    Validity arm_LDRBT() {
        return Validity::Unimplemented;
    }
    Validity arm_LDRHT() {
        return Validity::Unimplemented;
    }
    Validity arm_LDRSBT() {
        return Validity::Unimplemented;
    }
    Validity arm_LDRSHT() {
        return Validity::Unimplemented;
    }
    Validity arm_LDRT() {
        return Validity::Unimplemented;
    }
    Validity arm_STRBT() {
        return Validity::Unimplemented;
    }
    Validity arm_STRHT() {
        return Validity::Unimplemented;
    }
    Validity arm_STRT() {
        return Validity::Unimplemented;
    }
    Validity arm_LDR_lit(Cond cond, bool U, Reg t, Imm12 imm12) {
        if (t == Reg::PC) return Validity::ModifiesPC;
        return Validity::Valid;
    }
    Validity arm_LDR_imm(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm12 imm12) {
        if (n == Reg::PC) return Validity::Unpredictable;
        if (!P && W) return Validity::Unimplemented;
        if ((!P || W) && n == t) return Validity::Unpredictable;
        if (t == Reg::PC) return Validity::ModifiesPC;
        return Validity::Valid;
    }
    Validity arm_LDR_reg(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm5 imm5, ShiftType shift, Reg m) {
        if (!P && W) return Validity::Unimplemented;
        if (m == Reg::PC) return Validity::Unpredictable;
        if ((!P || W) && (n == Reg::PC || n == t)) return Validity::Unpredictable;
        if (t == Reg::PC) return Validity::ModifiesPC;
        return Validity::Valid;
    }
    Validity arm_LDRB_lit(Cond cond, bool U, Reg t, Imm12 imm12) {
        if (t == Reg::PC) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRB_imm(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm12 imm12) {
        if (n == Reg::PC) return Validity::Unpredictable;
        if (!P && W) return Validity::Unimplemented;
        if ((!P || W) && n == t) return Validity::Unpredictable;
        if (t == Reg::PC) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRB_reg(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm5 imm5, ShiftType shift, Reg m) {
        if (!P && W) return Validity::Unimplemented;
        if (t == Reg::PC || m == Reg::PC) return Validity::Unpredictable;
        if ((!P || W) && (n == Reg::PC || n == t)) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRD_lit(Cond cond, bool U, Reg t, Imm4 imm8a, Imm4 imm8b) {
        if (RegNumber(t) % 2 == 1) return Validity::Unpredictable;
        if (t+1 == Reg::PC) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRD_imm(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm4 imm8a, Imm4 imm8b) {
        if (n == Reg::PC) return Validity::Unpredictable;
        if (RegNumber(t) % 2 == 1) return Validity::Unpredictable;
        if (!P && W) return Validity::Unpredictable;
        if ((!P || W) && (n == t || n == t+1)) return Validity::Unpredictable;
        if (t+1 == Reg::PC) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRD_reg(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Reg m) {
        if (RegNumber(t) % 2 == 1) return Validity::Unpredictable;
        if (!P && W) return Validity::Unpredictable;
        if (t+1 == Reg::PC || m == Reg::PC || m == t || m == t+1) return Validity::Unpredictable;
        if ((!P || W) && (n == Reg::PC || n == t || n == t+1)) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRH_lit(Cond cond, bool P, bool U, bool W, Reg t, Imm4 imm8a, Imm4 imm8b) {
        if (!P && W) return Validity::Unimplemented;
        if (P == W) return Validity::Unpredictable;
        if (t == Reg::PC) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRH_imm(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm4 imm8a, Imm4 imm8b) {
        if (n == Reg::PC) return Validity::Unpredictable;
        if (!P && W) return Validity::Unimplemented;
        if ((!P || W) && n == t) return Validity::Unpredictable;
        if (t == Reg::PC) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRH_reg(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Reg m) {
        if (!P && W) return Validity::Unimplemented;
        if (t == Reg::PC || m == Reg::PC) return Validity::Unpredictable;
        if ((!P || W) && (n == Reg::PC || n == t)) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRSB_lit(Cond cond, bool U, Reg t, Imm4 imm8a, Imm4 imm8b) {
        if (t == Reg::PC) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRSB_imm(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm4 imm8a, Imm4 imm8b) {
        if (n == Reg::PC) return Validity::Unpredictable;
        if (!P && W) return Validity::Unimplemented;
        if ((!P || W) && n == t) return Validity::Unpredictable;
        if (t == Reg::PC) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRSB_reg(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Reg m) {
        if (!P && W) return Validity::Unimplemented;
        if (t == Reg::PC || m == Reg::PC) return Validity::Unpredictable;
        if ((!P || W) && (n == Reg::PC || n == t)) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRSH_lit(Cond cond, bool U, Reg t, Imm4 imm8a, Imm4 imm8b) {
        if (t == Reg::PC) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRSH_imm(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm4 imm8a, Imm4 imm8b) {
        if (n == Reg::PC) return Validity::Unpredictable;
        if (!P && W) return Validity::Unimplemented;
        if ((!P || W) && n == t) return Validity::Unpredictable;
        if (t == Reg::PC) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_LDRSH_reg(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Reg m) {
        if (!P && W) return Validity::Unimplemented;
        if (t == Reg::PC || m == Reg::PC) return Validity::Unpredictable;
        if ((!P || W) && (n == Reg::PC || n == t)) return Validity::Unpredictable;
        return Validity::Valid;
    }
    Validity arm_STR_imm(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm12 imm12) {
        return Validity::TODO;
    }
    Validity arm_STR_reg(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm5 imm5, ShiftType shift, Reg m) {
        return Validity::TODO;
    }
    Validity arm_STRB_imm(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm12 imm12) {
        return Validity::TODO;
    }
    Validity arm_STRB_reg(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm5 imm5, ShiftType shift, Reg m) {
        return Validity::TODO;
    }
    Validity arm_STRD_imm(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm4 imm8a, Imm4 imm8b) {
        return Validity::TODO;
    }
    Validity arm_STRD_reg(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Reg m) {
        return Validity::TODO;
    }
    Validity arm_STRH_imm(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Imm4 imm8a, Imm4 imm8b) {
        return Validity::TODO;
    }
    Validity arm_STRH_reg(Cond cond, bool P, bool U, bool W, Reg n, Reg t, Reg m) {
        return Validity::TODO;
    }

    // Load/Store multiple instructions
    Validity arm_LDM(Cond cond, bool W, Reg n, RegList list) {
        return n == Reg::PC || Common::BitCount(list) < 1 ? Validity::Unpredictable : Common::Bit<15>(list) ? Validity::ModifiesPC : Validity::Valid;
    }
    Validity arm_LDMDA(Cond cond, bool W, Reg n, RegList list) {
        return n == Reg::PC || Common::BitCount(list) < 1 ? Validity::Unpredictable : Common::Bit<15>(list) ? Validity::ModifiesPC : Validity::Valid;
    }
    Validity arm_LDMDB(Cond cond, bool W, Reg n, RegList list) {
        return n == Reg::PC || Common::BitCount(list) < 1 ? Validity::Unpredictable : Common::Bit<15>(list) ? Validity::ModifiesPC : Validity::Valid;
    }
    Validity arm_LDMIB(Cond cond, bool W, Reg n, RegList list) {
        return n == Reg::PC || Common::BitCount(list) < 1 ? Validity::Unpredictable : Common::Bit<15>(list) ? Validity::ModifiesPC : Validity::Valid;
    }
    Validity arm_LDM_usr() {
        return Validity::Unimplemented;
    }
    Validity arm_LDM_eret() {
        return Validity::Unimplemented;
    }
    Validity arm_STM(Cond cond, bool W, Reg n, RegList list) {
        return n == Reg::PC || Common::BitCount(list) < 1 ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_STMDA(Cond cond, bool W, Reg n, RegList list) {
        return n == Reg::PC || Common::BitCount(list) < 1 ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_STMDB(Cond cond, bool W, Reg n, RegList list) {
        return n == Reg::PC || Common::BitCount(list) < 1 ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_STMIB(Cond cond, bool W, Reg n, RegList list) {
        return n == Reg::PC || Common::BitCount(list) < 1 ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_STM_usr() {
        return Validity::Unimplemented;
    }

    // Miscellaneous instructions
    Validity arm_CLZ(Cond cond, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_NOP() {
        return Validity::Valid;
    }
    Validity arm_SEL(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }

    // Unsigned sum of absolute difference functions
    Validity arm_USAD8(Cond cond, Reg d, Reg m, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_USADA8(Cond cond, Reg d, Reg a, Reg m, Reg n) {
        return Validity::Unimplemented;
    }

    // Packing instructions
    Validity arm_PKHBT(Cond cond, Reg n, Reg d, Imm5 imm5, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_PKHTB(Cond cond, Reg n, Reg d, Imm5 imm5, Reg m) {
        return Validity::Unimplemented;
    }

    // Reversal instructions
    Validity arm_REV(Cond cond, Reg d, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_REV16(Cond cond, Reg d, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_REVSH(Cond cond, Reg d, Reg m) {
        return d == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }

    // Saturation instructions
    Validity arm_SSAT(Cond cond, Imm5 sat_imm, Reg d, Imm5 imm5, bool sh, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_SSAT16(Cond cond, Imm4 sat_imm, Reg d, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_USAT(Cond cond, Imm5 sat_imm, Reg d, Imm5 imm5, bool sh, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_USAT16(Cond cond, Imm4 sat_imm, Reg d, Reg n) {
        return Validity::Unimplemented;
    }

    // Multiply (Normal) instructions
    Validity arm_MLA(Cond cond, bool S, Reg d, Reg a, Reg m, Reg n) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC || a == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_MUL(Cond cond, bool S, Reg d, Reg m, Reg n) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }

    // Multiply (Long) instructions
    Validity arm_SMLAL(Cond cond, bool S, Reg dHi, Reg dLo, Reg m, Reg n) {
        return dLo == Reg::PC || dHi == Reg::PC || n == Reg::PC || m == Reg::PC || dLo == dHi ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SMULL(Cond cond, bool S, Reg dHi, Reg dLo, Reg m, Reg n) {
        return dLo == Reg::PC || dHi == Reg::PC || n == Reg::PC || m == Reg::PC || dLo == dHi ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UMAAL(Cond cond, Reg dHi, Reg dLo, Reg m, Reg n) {
        return dLo == Reg::PC || dHi == Reg::PC || n == Reg::PC || m == Reg::PC || dLo == dHi ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UMLAL(Cond cond, bool S, Reg dHi, Reg dLo, Reg m, Reg n) {
        return dLo == Reg::PC || dHi == Reg::PC || n == Reg::PC || m == Reg::PC || dLo == dHi ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UMULL(Cond cond, bool S, Reg dHi, Reg dLo, Reg m, Reg n) {
        return dLo == Reg::PC || dHi == Reg::PC || n == Reg::PC || m == Reg::PC || dLo == dHi ? Validity::Unpredictable : Validity::Valid;
    }

    // Multiply (Halfword) instructions
    Validity arm_SMLALxy(Cond cond, Reg dHi, Reg dLo, Reg m, bool M, bool N, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_SMLAxy(Cond cond, Reg d, Reg a, Reg m, bool M, bool N, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_SMULxy(Cond cond, Reg d, Reg m, bool M, bool N, Reg n) {
        return Validity::Unimplemented;
    }

    // Multiply (word by halfword) instructions
    Validity arm_SMLAWy(Cond cond, Reg d, Reg a, Reg m, bool M, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_SMULWy(Cond cond, Reg d, Reg m, bool M, Reg n) {
        return Validity::Unimplemented;
    }

    // Multiply (Most significant word) instructions
    Validity arm_SMMLA(Cond cond, Reg d, Reg a, Reg m, bool R, Reg n) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SMMLS(Cond cond, Reg d, Reg a, Reg m, bool R, Reg n) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC || a == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SMMUL(Cond cond, Reg d, Reg m, bool R, Reg n) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }

    // Multiply (Dual) instructions
    Validity arm_SMLAD(Cond cond, Reg d, Reg a, Reg m, bool M, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_SMLALD(Cond cond, Reg dHi, Reg dLo, Reg m, bool M, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_SMLSD(Cond cond, Reg d, Reg a, Reg m, bool M, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_SMLSLD(Cond cond, Reg dHi, Reg dLo, Reg m, bool M, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_SMUAD(Cond cond, Reg d, Reg m, bool M, Reg n) {
        return Validity::Unimplemented;
    }
    Validity arm_SMUSD(Cond cond, Reg d, Reg m, bool M, Reg n) {
        return Validity::Unimplemented;
    }

    // Parallel Add/Subtract (Modulo arithmetic) instructions
    Validity arm_SADD8(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_SADD16(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_SASX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_SSAX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_SSUB8(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_SSUB16(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_UADD8(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_UADD16(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_UASX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_USAX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_USUB8(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_USUB16(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }

    // Parallel Add/Subtract (Saturating) instructions
    Validity arm_QADD8(Cond cond, Reg n, Reg d, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_QADD16(Cond cond, Reg n, Reg d, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_QASX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_QSAX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_QSUB8(Cond cond, Reg n, Reg d, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_QSUB16(Cond cond, Reg n, Reg d, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UQADD8(Cond cond, Reg n, Reg d, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UQADD16(Cond cond, Reg n, Reg d, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UQASX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_UQSAX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_UQSUB8(Cond cond, Reg n, Reg d, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_UQSUB16(Cond cond, Reg n, Reg d, Reg m) {
        return d == Reg::PC || n == Reg::PC || m == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }

    // Parallel Add/Subtract (Halving) instructions
    Validity arm_SHADD8(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_SHADD16(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_SHASX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_SHSAX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_SHSUB8(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_SHSUB16(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_UHADD8(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_UHADD16(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_UHASX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_UHSAX(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_UHSUB8(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_UHSUB16(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }

    // Saturated Add/Subtract instructions
    Validity arm_QADD(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_QSUB(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_QDADD(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }
    Validity arm_QDSUB(Cond cond, Reg n, Reg d, Reg m) {
        return Validity::Unimplemented;
    }

    // Synchronization Primitive instructions
    Validity arm_CLREX() {
        return Validity::Valid;
    }
    Validity arm_LDREX(Cond cond, Reg n, Reg d) {
        return d == Reg::PC || n == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_LDREXB(Cond cond, Reg n, Reg d) {
        return d == Reg::PC || n == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_LDREXD(Cond cond, Reg n, Reg d) {
        return d == Reg::LR || d == Reg::PC || n == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_LDREXH(Cond cond, Reg n, Reg d) {
        return d == Reg::PC || n == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_STREX(Cond cond, Reg n, Reg d, Reg m) {
        return n == Reg::PC || d == Reg::PC || m == Reg::PC || d == n || d == m ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_STREXB(Cond cond, Reg n, Reg d, Reg m) {
        return n == Reg::PC || d == Reg::PC || m == Reg::PC || d == n || d == m  ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_STREXD(Cond cond, Reg n, Reg d, Reg m) {
        return n == Reg::PC || d == Reg::PC || m == Reg::LR || static_cast<size_t>(m) % 2 == 1 || d == n || d == m || d == m+1 ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_STREXH(Cond cond, Reg n, Reg d, Reg m) {
        return n == Reg::PC || d == Reg::PC || m == Reg::PC || d == n || d == m ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SWP(Cond cond, Reg n, Reg t, Reg t2) {
        return t == Reg::PC || t2 == Reg::PC || n == Reg::PC || n == t || n == t2 ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_SWPB(Cond cond, Reg n, Reg t, Reg t2) {
        return t == Reg::PC || t2 == Reg::PC || n == Reg::PC || n == t || n == t2 ? Validity::Unpredictable : Validity::Valid;
    }

    // Status register access instructions
    Validity arm_CPS() {
        return Validity::Unimplemented;
    }
    Validity arm_MRS(Cond cond, Reg d) {
        return d == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_MSR_imm(Cond cond, int mask, int rotate, Imm8 imm8) {
        return mask == 0b00 ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_MSR_reg(Cond cond, int mask, Reg n) {
        return mask == 0b00 || n == Reg::PC ? Validity::Unpredictable : Validity::Valid;
    }
    Validity arm_RFE() {
        return Validity::Unimplemented;
    }
    Validity arm_SETEND(bool E) {
        return Validity::Valid;
    }
    Validity arm_SRS() {
        return Validity::Unimplemented;
    }
};

u32 GenerateInstruction(bool allow_pc_modification) {
    const static auto table = Arm::GetArmDecodeTable<ValidArmVisitor>();
    ValidArmVisitor visitor{};

    while (true) {
        const auto inst_type = table[RandInt<size_t>(0, table.size() - 1)];

    redo_current_inst_type:
        u32 cond = RandInt(0, 24) == 0 ? RandInt<u32>(0, 0xD) : 0xE; // cond == AL more often than not
        u32 rand = (cond << 28) | RandInt<u32>(0, 0x0FFFFFFF);
        u32 inst = inst_type.GetExpect() | (rand & ~inst_type.GetMask());

        switch (inst_type.call(visitor, inst)) {
        case Validity::Valid:
            return inst;
        case Validity::Unpredictable:
            goto redo_current_inst_type;
        case Validity::ModifiesPC:
            if (allow_pc_modification)
                return inst;
            continue;
        case Validity::Unimplemented:
        case Validity::TODO:
            continue;
        }
    }
}
