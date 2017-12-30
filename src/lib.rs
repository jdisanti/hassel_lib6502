//
// Copyright 2017 hassel_lib6502 Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.
//

//! The hassel_lib6502 crate provides all of the information you could need about both the
//! documented and undocumented instructions for the MOS 6502 processor for the purposes of
//! writing emulators, assemblers, disassemblers, or compilers.
//! 
//! ## Examples
//! 
//! Decoding an op from binary:
//! 
//! ```rust
//! # extern crate hassel_lib6502;
//! # use hassel_lib6502::{Op, OpCode, OpParam};
//! # fn main() {
//! let bytes = vec![0x20, 0x41, 0xE0];
//! 
//! let op_code = OpCode::from_value(bytes[0]).unwrap();
//! let op_param = match op_code.len {
//!     1 => OpParam::None,
//!     2 => OpParam::Byte(bytes[1]),
//!     3 => OpParam::Word((bytes[2] as u16) << 8 | (bytes[1] as u16)),
//!     _ => unreachable!()
//! };
//! let op = Op::new(op_code, op_param);
//! println!("{:#?}", op);
//! # }
//! ```
//! 
//! Creating an op based on class and address mode:
//! 
//! ```rust
//! # extern crate hassel_lib6502;
//! # use hassel_lib6502::{Op, OpAddressMode OpClass, OpCode, OpParam};
//! # fn main() {
//! let op_class = OpClass::Lda;
//! let address_mode = OpAddressMode::ZeroPage;
//! let op_code = OpCode::find_by_class_and_mode(op_class, address_mode).unwrap();
//! 
//! // This resultant op will be equivalent to LDA $00
//! let op = Op::new(op_code, OpParam::Byte(0x00));
//! # }
//! ```
//! 

#[macro_use]
extern crate lazy_static;

#[allow(unused)]
mod op;

pub use op::{Op, OpAddressMode, OpClass, OpCode, OpParam};