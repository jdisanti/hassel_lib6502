use std::collections::HashMap;
use std::ascii::AsciiExt;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum OpParam {
    None,
    Byte(u8),
    Word(u16),
}

impl OpParam {
    pub fn len(&self) -> u8 {
        match *self {
            OpParam::None => 0,
            OpParam::Byte(_) => 1,
            OpParam::Word(_) => 2,
        }
    }

    pub fn low_byte(&self) -> u8 {
        match *self {
            OpParam::None => panic!("can't take low_byte of none"),
            OpParam::Byte(_) => panic!("can't take low_byte of byte"),
            OpParam::Word(val) => val as u8,
        }
    }

    pub fn high_byte(&self) -> u8 {
        match *self {
            OpParam::None => panic!("can't take high_byte of none"),
            OpParam::Byte(_) => panic!("can't take high_byte of byte"),
            OpParam::Word(val) => (val >> 8) as u8,
        }
    }

    pub fn as_u8(&self) -> u8 {
        match *self {
            OpParam::None => panic!("can't make u8 from nothing"),
            OpParam::Byte(val) => val,
            OpParam::Word(val) => val as u8,
        }
    }

    pub fn as_u16(&self) -> u16 {
        match *self {
            OpParam::None => panic!("can't make u16 from nothing"),
            OpParam::Byte(val) => val as u16,
            OpParam::Word(val) => val,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum OpAddressMode {
    Implied,
    Immediate,
    Absolute,
    AbsoluteOffsetX,
    AbsoluteOffsetY,
    ZeroPage,
    ZeroPageOffsetX,
    ZeroPageOffsetY,
    PCOffset,
    Indirect,
    PreIndirectX,
    PostIndirectY,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum OpClass {
    Nop, Top, Brk,
    Clc, Cld, Cli, Clv, Sec, Sed, Sei,
    Lda, Ldx, Ldy,
    Sta, Stx, Sty,
    Pha, Php, Pla, Plp,
    Tax, Tay, Tsx, Txa, Txs, Tya,
    Bit,
    Cmp, Cpx, Cpy,
    Bcc, Bcs, Beq, Bmi, Bne, Bpl, Bvc, Bvs,
    JmpAbs, JmpIndirect, Jsr,
    Rti, Rts,
    And, Asl, Eor, Lsr, Ora, Rol, Ror,
    Adc, Dec, Dex, Dey, Inc, Inx, Iny, Sbc,
}

impl OpClass {
    pub fn from_name(name: &str) -> Option<OpClass> {
        OP_CLASS_BY_NAME.get(&name.to_ascii_lowercase() as &str).map(|v| *v)
    }
}

/// Struct representing an opcode for the MOS 6502 processor
/// Includes all information about the opcode except for its parameter
#[derive(Debug, Eq, PartialEq)]
pub struct OpCode {
    /// The byte-value for this particular op-code
    /// For example, for a NOP, this would be 0xEA
    pub value: u8,

    /// The ALLCAPS name of the OpCode
    pub name: &'static str,

    /// The class of the opcode for convenience
    pub class: OpClass,

    /// How many bytes the opcode should occupy including its parameter
    pub len: u8,

    /// The minimum number of cycles the opcode takes to execute
    pub base_cycles: u8,

    /// The address mode the opcode uses with the parameter
    pub address_mode: OpAddressMode,
}

impl OpCode {
    /// Returns an opcode based off its byte value
    pub fn from_value(value: u8) -> Option<&'static OpCode> {
        OP_CODES_BY_VALUE.get(&value).map(|v| *v)
    }

    pub fn find_by_class_and_mode(class: OpClass, mode: OpAddressMode) -> Option<&'static OpCode> {
        for op_code in OP_CODES.iter() {
            if op_code.class == class && op_code.address_mode == mode {
                return Some(op_code)
            }
        }
        None
    }
}

/// Struct representing an opcode and its parameter
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Op {
    pub code: &'static OpCode,
    pub param: OpParam,
}

impl Op {
    pub fn new(code: &'static OpCode, param: OpParam) -> Op {
        if code.len != param.len() + 1 {
            panic!("Can't construct Op: OpCode requires a parameter of length {}, but given length {}", code.len - 1, param.len());
        } else {
            Op {
                code: code,
                param: param,
            }
        }
    }
}

macro_rules! configure_opcodes {
    ( ($vec_name:ident) {
        $( $value:expr => ($name:expr, $class:ident, $len:expr, $cycles:expr, $mode:expr) ),*
    } ) => {
        $(
            $vec_name.push(OpCode {
                value: $value,
                name: $name,
                class: $class,
                len: $len,
                base_cycles: $cycles,
                address_mode: $mode,
            });
        )*
    }
}

lazy_static! {
    static ref OP_CODES_BY_VALUE: HashMap<u8, &'static OpCode> = {
        let mut map: HashMap<u8, &'static OpCode> = HashMap::new();
        for op_code in OP_CODES.iter() {
            map.insert(op_code.value, &op_code);
        }
        map
    };

    static ref OP_CLASS_BY_NAME: HashMap<&'static str, OpClass> = {
        use OpClass::*;

        let mut map = HashMap::new();
        map.insert("nop", Nop);
        map.insert("brk", Brk);
        map.insert("clc", Clc);
        map.insert("cld", Cld);
        map.insert("cli", Cli);
        map.insert("clv", Clv);
        map.insert("sec", Sec);
        map.insert("sed", Sed);
        map.insert("sei", Sei);
        map.insert("lda", Lda);
        map.insert("ldx", Ldx);
        map.insert("ldy", Ldy);
        map.insert("sta", Sta);
        map.insert("stx", Stx);
        map.insert("sty", Sty);
        map.insert("pha", Pha);
        map.insert("php", Php);
        map.insert("pla", Pla);
        map.insert("plp", Plp);
        map.insert("tax", Tax);
        map.insert("tay", Tay);
        map.insert("tsx", Tsx);
        map.insert("txa", Txa);
        map.insert("txs", Txs);
        map.insert("tya", Tya);
        map.insert("bit", Bit);
        map.insert("cmp", Cmp);
        map.insert("cpx", Cpx);
        map.insert("cpy", Cpy);
        map.insert("bcc", Bcc);
        map.insert("bcs", Bcs);
        map.insert("beq", Beq);
        map.insert("bmi", Bmi);
        map.insert("bne", Bne);
        map.insert("bpl", Bpl);
        map.insert("bvc", Bvc);
        map.insert("bvs", Bvs);
        // TODO: Figure out jmp since it has two classes
        // map.insert("jmp", JmpAbs);
        map.insert("jsr", Jsr);
        map.insert("rti", Rti);
        map.insert("rts", Rts);
        map.insert("and", And);
        map.insert("asl", Asl);
        map.insert("eor", Eor);
        map.insert("lsr", Lsr);
        map.insert("ora", Ora);
        map.insert("rol", Rol);
        map.insert("ror", Ror);
        map.insert("adc", Adc);
        map.insert("dec", Dec);
        map.insert("dex", Dex);
        map.insert("dey", Dey);
        map.insert("inc", Inc);
        map.insert("inx", Inx);
        map.insert("iny", Iny);
        map.insert("sbc", Sbc);
        map
    };

    static ref OP_CODES: Vec<OpCode> = {
        use self::OpClass::*;
        use self::OpAddressMode::*;

        let mut op_codes = Vec::new();

        configure_opcodes!((op_codes) {
            0xEA => ("NOP", Nop, 1, 2, Implied),
            0x1A => ("NOP", Nop, 1, 2, Implied),
            0x3A => ("NOP", Nop, 1, 2, Implied),
            0x5A => ("NOP", Nop, 1, 2, Implied),
            0x7A => ("NOP", Nop, 1, 2, Implied),
            0xDA => ("NOP", Nop, 1, 2, Implied),
            0xFA => ("NOP", Nop, 1, 2, Implied),

            // Undocumented "double NOPs"
            0x04 => ("DOP", Nop, 2, 3, ZeroPage),
            0x14 => ("DOP", Nop, 2, 4, ZeroPageOffsetX),
            0x34 => ("DOP", Nop, 2, 4, ZeroPageOffsetX),
            0x44 => ("DOP", Nop, 2, 3, ZeroPage),
            0x54 => ("DOP", Nop, 2, 4, ZeroPageOffsetX),
            0x64 => ("DOP", Nop, 2, 3, ZeroPage),
            0x74 => ("DOP", Nop, 2, 4, ZeroPageOffsetX),
            0x80 => ("DOP", Nop, 2, 2, Immediate),
            0x82 => ("DOP", Nop, 2, 2, Immediate),
            0x89 => ("DOP", Nop, 2, 2, Immediate),
            0xC2 => ("DOP", Nop, 2, 2, Immediate),
            0xD4 => ("DOP", Nop, 2, 4, ZeroPageOffsetX),
            0xE2 => ("DOP", Nop, 2, 2, Immediate),
            0xF4 => ("DOP", Nop, 2, 4, ZeroPageOffsetX),

            // Undocumented "tripple NOPs"
            0x0C => ("TOP", Nop, 3, 4, Absolute),
            0x1C => ("TOP", Top, 3, 4, AbsoluteOffsetX),
            0x3C => ("TOP", Top, 3, 4, AbsoluteOffsetX),
            0x5C => ("TOP", Top, 3, 4, AbsoluteOffsetX),
            0x7C => ("TOP", Top, 3, 4, AbsoluteOffsetX),
            0xDC => ("TOP", Top, 3, 4, AbsoluteOffsetX),
            0xFC => ("TOP", Top, 3, 4, AbsoluteOffsetX),

            0x00 => ("BRK", Brk, 1, 7, Implied),

            // Flag modifiers
            0x18 => ("CLC", Clc, 1, 2, Implied),
            0xD8 => ("CLD", Cld, 1, 2, Implied),
            0x58 => ("CLI", Cli, 1, 2, Implied),
            0xB8 => ("CLV", Clv, 1, 2, Implied),
            0x38 => ("SEC", Sec, 1, 2, Implied),
            0xF8 => ("SED", Sed, 1, 2, Implied),
            0x78 => ("SEI", Sei, 1, 2, Implied),

            // LDA
            0xA9 => ("LDA", Lda, 2, 2, Immediate),
            0xA5 => ("LDA", Lda, 2, 3, ZeroPage),
            0xB5 => ("LDA", Lda, 2, 4, ZeroPageOffsetX),
            0xAD => ("LDA", Lda, 3, 4, Absolute),
            0xBD => ("LDA", Lda, 3, 4, AbsoluteOffsetX),
            0xB9 => ("LDA", Lda, 3, 4, AbsoluteOffsetY),
            0xA1 => ("LDA", Lda, 2, 6, PreIndirectX),
            0xB1 => ("LDA", Lda, 2, 5, PostIndirectY),

            // LDX
            0xA2 => ("LDX", Ldx, 2, 2, Immediate),
            0xA6 => ("LDX", Ldx, 2, 3, ZeroPage),
            0xB6 => ("LDX", Ldx, 2, 4, ZeroPageOffsetY),
            0xAE => ("LDX", Ldx, 3, 4, Absolute),
            0xBE => ("LDX", Ldx, 3, 4 /* (+1) */, AbsoluteOffsetY),

            // LDY
            0xA0 => ("LDY", Ldy, 2, 2, Immediate),
            0xA4 => ("LDY", Ldy, 2, 3, ZeroPage),
            0xB4 => ("LDY", Ldy, 2, 4, ZeroPageOffsetX),
            0xAC => ("LDY", Ldy, 3, 4, Absolute),
            0xBC => ("LDY", Ldy, 3, 4 /* (+1) */, AbsoluteOffsetX),

            // STA
            0x85 => ("STA", Sta, 2, 3, ZeroPage),
            0x95 => ("STA", Sta, 2, 4, ZeroPageOffsetX),
            0x8D => ("STA", Sta, 3, 4, Absolute),
            0x9D => ("STA", Sta, 3, 5, AbsoluteOffsetX),
            0x99 => ("STA", Sta, 3, 5, AbsoluteOffsetY),
            0x81 => ("STA", Sta, 2, 6, PreIndirectX),
            0x91 => ("STA", Sta, 2, 6, PostIndirectY),

            // STX
            0x86 => ("STX", Stx, 2, 3, ZeroPage),
            0x96 => ("STX", Stx, 2, 4, ZeroPageOffsetY),
            0x8E => ("STX", Stx, 3, 4, Absolute),

            // STY
            0x84 => ("STY", Sty, 2, 3, ZeroPage),
            0x94 => ("STY", Sty, 2, 4, ZeroPageOffsetX),
            0x8C => ("STY", Sty, 3, 4, Absolute),

            // Stack
            0x48 => ("PHA", Pha, 1, 3, Implied),
            0x08 => ("PHP", Php, 1, 3, Implied),
            0x68 => ("PLA", Pla, 1, 4, Implied),
            0x28 => ("PLP", Plp, 1, 4, Implied),

            // Transfer
            0xAA => ("TAX", Tax, 1, 2, Implied),
            0xA8 => ("TAY", Tay, 1, 2, Implied),
            0xBA => ("TSX", Tsx, 1, 2, Implied),
            0x8A => ("TXA", Txa, 1, 2, Implied),
            0x9A => ("TXS", Txs, 1, 2, Implied),
            0x98 => ("TYA", Tya, 1, 2, Implied),

            ///////////////////////////////////////
            // Compare
            ///////////////////////////////////////

            // BIT
            0x24 => ("BIT", Bit, 2, 3, ZeroPage),
            0x2C => ("BIT", Bit, 3, 4, Absolute),

            // CMP
            0xC9 => ("CMP", Cmp, 2, 2, Immediate),
            0xC5 => ("CMP", Cmp, 2, 3, ZeroPage),
            0xD5 => ("CMP", Cmp, 2, 4, ZeroPageOffsetX),
            0xCD => ("CMP", Cmp, 3, 4, Absolute),
            0xDD => ("CMP", Cmp, 3, 4 /* (+1) */, AbsoluteOffsetX),
            0xD9 => ("CMP", Cmp, 3, 4 /* (+1) */, AbsoluteOffsetY),
            0xC1 => ("CMP", Cmp, 2, 6, PreIndirectX),
            0xD1 => ("CMP", Cmp, 2, 5 /* (+1) */, PostIndirectY),

            // CPX
            0xE0 => ("CPX", Cpx, 2, 2, Immediate),
            0xE4 => ("CPX", Cpx, 2, 3, ZeroPage),
            0xEC => ("CPX", Cpx, 3, 4, Absolute),

            // CPY
            0xC0 => ("CPY", Cpy, 2, 2, Immediate),
            0xC4 => ("CPY", Cpy, 2, 3, ZeroPage),
            0xCC => ("CPY", Cpy, 3, 4, Absolute),

            ///////////////////////////////////////
            // Branch
            ///////////////////////////////////////

            // Branch iff
            0x90 => ("BCC", Bcc, 2, 2, PCOffset),
            0xB0 => ("BCS", Bcs, 2, 2, PCOffset),
            0xF0 => ("BEQ", Beq, 2, 2, PCOffset),
            0x30 => ("BMI", Bmi, 2, 2, PCOffset),
            0xD0 => ("BNE", Bne, 2, 2, PCOffset),
            0x10 => ("BPL", Bpl, 2, 2, PCOffset),
            0x50 => ("BVC", Bvc, 2, 2, PCOffset),
            0x70 => ("BVS", Bvs, 2, 2, PCOffset),

            // Jump
            0x4C => ("JMP", JmpAbs, 3, 3, Absolute),
            0x6C => ("JMP", JmpIndirect, 3, 5, Indirect),
            0x20 => ("JSR", Jsr, 3, 6, Absolute),

            // Return
            0x40 => ("RTI", Rti, 1, 6, Implied),
            0x60 => ("RTS", Rts, 1, 6, Implied),

            ///////////////////////////////////////
            // Bitwise
            ///////////////////////////////////////

            // AND
            0x29 => ("AND", And, 2, 2, Immediate),
            0x25 => ("AND", And, 2, 3, ZeroPage),
            0x35 => ("AND", And, 2, 4, ZeroPageOffsetX),
            0x2D => ("AND", And, 3, 4, Absolute),
            0x3D => ("AND", And, 3, 4 /* (+1) */, AbsoluteOffsetX),
            0x39 => ("AND", And, 3, 4 /* (+1) */, AbsoluteOffsetY),
            0x21 => ("AND", And, 2, 6, PreIndirectX),
            0x31 => ("AND", And, 2, 5 /* (+1) */, PostIndirectY),

            // ASL
            0x0A => ("ASL A", Asl, 1, 2, Implied),
            0x06 => ("ASL", Asl, 2, 5, ZeroPage),
            0x16 => ("ASL", Asl, 2, 6, ZeroPageOffsetX),
            0x0E => ("ASL", Asl, 3, 6, Absolute),
            0x1E => ("ASL", Asl, 3, 7, AbsoluteOffsetX),

            // EOR
            0x49 => ("EOR", Eor, 2, 2, Immediate),
            0x45 => ("EOR", Eor, 2, 3, ZeroPage),
            0x55 => ("EOR", Eor, 2, 4, ZeroPageOffsetX),
            0x4D => ("EOR", Eor, 3, 4, Absolute),
            0x5D => ("EOR", Eor, 3, 4 /* (+1) */, AbsoluteOffsetX),
            0x59 => ("EOR", Eor, 3, 4 /* (+1) */, AbsoluteOffsetY),
            0x41 => ("EOR", Eor, 2, 6, PreIndirectX),
            0x51 => ("EOR", Eor, 2, 5 /* (+1) */, PostIndirectY),

            // LSR
            0x4A => ("LSR A", Lsr, 1, 2, Implied),
            0x46 => ("LSR", Lsr, 2, 5, ZeroPage),
            0x56 => ("LSR", Lsr, 2, 6, ZeroPageOffsetX),
            0x4E => ("LSR", Lsr, 3, 6, Absolute),
            0x5E => ("LSR", Lsr, 3, 7, AbsoluteOffsetX),

            // ORA
            0x09 => ("ORA", Ora, 2, 2, Immediate),
            0x05 => ("ORA", Ora, 2, 3, ZeroPage),
            0x15 => ("ORA", Ora, 2, 4, ZeroPageOffsetX),
            0x0D => ("ORA", Ora, 3, 4, Absolute),
            0x1D => ("ORA", Ora, 3, 4 /* (+1) */, AbsoluteOffsetX),
            0x19 => ("ORA", Ora, 3, 4 /* (+1) */, AbsoluteOffsetY),
            0x01 => ("ORA", Ora, 2, 6, PreIndirectX),
            0x11 => ("ORA", Ora, 2, 5 /* (+1) */, PostIndirectY),

            // ROL
            0x2A => ("ROL A", Rol, 1, 2, Implied),
            0x26 => ("ROL", Rol, 2, 5, ZeroPage),
            0x36 => ("ROL", Rol, 2, 6, ZeroPageOffsetX),
            0x2E => ("ROL", Rol, 3, 6, Absolute),
            0x3E => ("ROL", Rol, 3, 7, AbsoluteOffsetX),

            // ROR
            0x6A => ("ROR A", Ror, 1, 2, Implied),
            0x66 => ("ROR", Ror, 2, 5, ZeroPage),
            0x76 => ("ROR", Ror, 2, 6, ZeroPageOffsetX),
            0x6E => ("ROR", Ror, 3, 6, Absolute),
            0x7E => ("ROR", Ror, 3, 7, AbsoluteOffsetX),

            ///////////////////////////////////////
            // Arithmetic
            ///////////////////////////////////////

            // ADC
            0x69 => ("ADC", Adc, 2, 2, Immediate),
            0x65 => ("ADC", Adc, 2, 3, ZeroPage),
            0x75 => ("ADC", Adc, 2, 4, ZeroPageOffsetX),
            0x6D => ("ADC", Adc, 3, 4, Absolute),
            0x7D => ("ADC", Adc, 3, 4 /* (+1) */, AbsoluteOffsetX),
            0x79 => ("ADC", Adc, 3, 4 /* (+1) */, AbsoluteOffsetY),
            0x61 => ("ADC", Adc, 2, 6, PreIndirectX),
            0x71 => ("ADC", Adc, 2, 5 /* (+1) */, PostIndirectY),

            // DEC
            0xC6 => ("DEC", Dec, 2, 5, ZeroPage),
            0xD6 => ("DEC", Dec, 2, 6, ZeroPageOffsetX),
            0xCE => ("DEC", Dec, 3, 6, Absolute),
            0xDE => ("DEC", Dec, 3, 7, AbsoluteOffsetX),

            0xCA => ("DEX", Dex, 1, 2, Implied),
            0x88 => ("DEY", Dey, 1, 2, Implied),

            // INC
            0xE6 => ("INC", Inc, 2, 5, ZeroPage),
            0xF6 => ("INC", Inc, 2, 6, ZeroPageOffsetX),
            0xEE => ("INC", Inc, 3, 6, Absolute),
            0xFE => ("INC", Inc, 3, 7, AbsoluteOffsetX),

            0xE8 => ("INX", Inx, 1, 2, Implied),
            0xC8 => ("INY", Iny, 1, 2, Implied),

            // SBC
            0xE9 => ("SBC", Sbc, 2, 2, Immediate),
            0xEB => ("SBC", Sbc, 2, 2, Immediate), // unofficial opcode same as 0xE9
            0xE5 => ("SBC", Sbc, 2, 3, ZeroPage),
            0xF5 => ("SBC", Sbc, 2, 4, ZeroPageOffsetX),
            0xED => ("SBC", Sbc, 3, 4, Absolute),
            0xFD => ("SBC", Sbc, 3, 4 /* (+1) */, AbsoluteOffsetX),
            0xF9 => ("SBC", Sbc, 3, 4 /* (+1) */, AbsoluteOffsetY),
            0xE1 => ("SBC", Sbc, 2, 6, PreIndirectX),
            0xF1 => ("SBC", Sbc, 2, 5 /* (+1) */, PostIndirectY)
        });

        op_codes
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sanity_test() {
        let nop = OpCode::from_value(0xEA).unwrap();
        assert_eq!("NOP", nop.name);

        let nop = OpClass::from_name("NOP").unwrap();
        assert_eq!(OpClass::Nop, nop);
    }
}