//! Utilities for IR generation and register allocation
//! 
//! This module provides utilities that make it easy to extend the IR
//! with register allocation and other optimizations.

use std::collections::{HashMap, HashSet};
use super::{BasicBlock, Function, IR, Temp};

/// Register allocation interface - designed for easy extensibility
pub trait RegisterAllocator {
    /// Allocate physical registers for the given function
    fn allocate_registers(&mut self, function: &Function) -> HashMap<Temp, PhysicalRegister>;
    
    /// Get the number of spill slots needed
    fn get_spill_slots(&self) -> usize;
}

/// Represents a physical register (can be extended for different architectures)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum PhysicalRegister {
    // RISC-V registers
    X0,  // zero
    X1,  // ra (return address)
    X2,  // sp (stack pointer)
    X3,  // gp (global pointer)
    X4,  // tp (thread pointer)
    X5,  // t0 (temporary)
    X6,  // t1
    X7,  // t2
    X8,  // s0/fp (saved/frame pointer)
    X9,  // s1 (saved)
    X10, // a0 (argument/return)
    X11, // a1
    X12, // a2
    X13, // a3
    X14, // a4
    X15, // a5
    X16, // a6
    X17, // a7
    X18, // s2 (saved)
    X19, // s3
    X20, // s4
    X21, // s5
    X22, // s6
    X23, // s7
    X24, // s8
    X25, // s9
    X26, // s10
    X27, // s11
    X28, // t3 (temporary)
    X29, // t4
    X30, // t5
    X31, // t6
    
    // Floating point registers
    F0, F1, F2, F3, F4, F5, F6, F7,
    F8, F9, F10, F11, F12, F13, F14, F15,
    F16, F17, F18, F19, F20, F21, F22, F23,
    F24, F25, F26, F27, F28, F29, F30, F31,
    
    // Special case for spilled variables
    Spill(usize),
}

impl PhysicalRegister {
    /// Get the register name for assembly generation
    #[allow(dead_code)]
    pub fn to_asm_name(&self) -> &'static str {
        match self {
            PhysicalRegister::X0 => "x0",
            PhysicalRegister::X1 => "x1",
            PhysicalRegister::X2 => "x2",
            PhysicalRegister::X3 => "x3",
            PhysicalRegister::X4 => "x4",
            PhysicalRegister::X5 => "x5",
            PhysicalRegister::X6 => "x6",
            PhysicalRegister::X7 => "x7",
            PhysicalRegister::X8 => "x8",
            PhysicalRegister::X9 => "x9",
            PhysicalRegister::X10 => "x10",
            PhysicalRegister::X11 => "x11",
            PhysicalRegister::X12 => "x12",
            PhysicalRegister::X13 => "x13",
            PhysicalRegister::X14 => "x14",
            PhysicalRegister::X15 => "x15",
            PhysicalRegister::X16 => "x16",
            PhysicalRegister::X17 => "x17",
            PhysicalRegister::X18 => "x18",
            PhysicalRegister::X19 => "x19",
            PhysicalRegister::X20 => "x20",
            PhysicalRegister::X21 => "x21",
            PhysicalRegister::X22 => "x22",
            PhysicalRegister::X23 => "x23",
            PhysicalRegister::X24 => "x24",
            PhysicalRegister::X25 => "x25",
            PhysicalRegister::X26 => "x26",
            PhysicalRegister::X27 => "x27",
            PhysicalRegister::X28 => "x28",
            PhysicalRegister::X29 => "x29",
            PhysicalRegister::X30 => "x30",
            PhysicalRegister::X31 => "x31",
            PhysicalRegister::F0 => "f0",
            PhysicalRegister::F1 => "f1",
            PhysicalRegister::F2 => "f2",
            PhysicalRegister::F3 => "f3",
            PhysicalRegister::F4 => "f4",
            PhysicalRegister::F5 => "f5",
            PhysicalRegister::F6 => "f6",
            PhysicalRegister::F7 => "f7",
            PhysicalRegister::F8 => "f8",
            PhysicalRegister::F9 => "f9",
            PhysicalRegister::F10 => "f10",
            PhysicalRegister::F11 => "f11",
            PhysicalRegister::F12 => "f12",
            PhysicalRegister::F13 => "f13",
            PhysicalRegister::F14 => "f14",
            PhysicalRegister::F15 => "f15",
            PhysicalRegister::F16 => "f16",
            PhysicalRegister::F17 => "f17",
            PhysicalRegister::F18 => "f18",
            PhysicalRegister::F19 => "f19",
            PhysicalRegister::F20 => "f20",
            PhysicalRegister::F21 => "f21",
            PhysicalRegister::F22 => "f22",
            PhysicalRegister::F23 => "f23",
            PhysicalRegister::F24 => "f24",
            PhysicalRegister::F25 => "f25",
            PhysicalRegister::F26 => "f26",
            PhysicalRegister::F27 => "f27",
            PhysicalRegister::F28 => "f28",
            PhysicalRegister::F29 => "f29",
            PhysicalRegister::F30 => "f30",
            PhysicalRegister::F31 => "f31",
            PhysicalRegister::Spill(n) => panic!("Cannot get asm name for spill slot {}", n),
        }
    }
    
    /// Check if this is a caller-saved (temporary) register
    #[allow(dead_code)]
    pub fn is_caller_saved(&self) -> bool {
        matches!(self, 
            PhysicalRegister::X1 | // ra
            PhysicalRegister::X5 | PhysicalRegister::X6 | PhysicalRegister::X7 | // t0-t2
            PhysicalRegister::X10 | PhysicalRegister::X11 | // a0-a1
            PhysicalRegister::X12 | PhysicalRegister::X13 | // a2-a3
            PhysicalRegister::X14 | PhysicalRegister::X15 | // a4-a5
            PhysicalRegister::X16 | PhysicalRegister::X17 | // a6-a7
            PhysicalRegister::X28 | PhysicalRegister::X29 | // t3-t4
            PhysicalRegister::X30 | PhysicalRegister::X31   // t5-t6
        )
    }
    
    /// Check if this is a callee-saved register
    #[allow(dead_code)]
    pub fn is_callee_saved(&self) -> bool {
        matches!(self,
            PhysicalRegister::X8 | PhysicalRegister::X9 |   // s0-s1
            PhysicalRegister::X18 | PhysicalRegister::X19 | // s2-s3
            PhysicalRegister::X20 | PhysicalRegister::X21 | // s4-s5
            PhysicalRegister::X22 | PhysicalRegister::X23 | // s6-s7
            PhysicalRegister::X24 | PhysicalRegister::X25 | // s8-s9
            PhysicalRegister::X26 | PhysicalRegister::X27   // s10-s11
        )
    }
}

/// IR analysis utilities for register allocation and optimization
pub struct IRAnalyzer;

impl IRAnalyzer {
    /// Get all temporaries used in a basic block (used by register allocation)
    #[allow(dead_code)]
    pub fn get_used_temps(block: &BasicBlock) -> HashSet<Temp> {
        let mut temps = HashSet::new();
        
        for instr in block.get_instructions() {
            let instr_temps = Self::get_instruction_temps(instr);
            for temp in instr_temps {
                temps.insert(temp);
            }
        }
        
        temps
    }
    
    /// Get all temporaries defined (written to) in a basic block
    #[allow(dead_code)]
    pub fn get_defined_temps(block: &BasicBlock) -> HashSet<Temp> {
        let mut temps = HashSet::new();
        
        for instr in block.get_instructions() {
            match instr {
                IR::Binary { dest, .. } | 
                IR::Unary { dest, .. } |
                IR::Load { dest, .. } |
                IR::LoadImm { dest, .. } |
                IR::LoadFloat { dest, .. } |
                IR::Alloc { dest, .. } => {
                    temps.insert(dest.clone());
                }
                IR::Call { ret: Some(dest), .. } => {
                    temps.insert(dest.clone());
                }
                _ => {}
            }
        }
        
        temps
    }
    
    /// Calculate live ranges for all temporaries in a function
    /// Returns a map from Temp to (first_use, last_use) instruction indices
    pub fn calculate_live_ranges(function: &Function) -> HashMap<Temp, (usize, usize)> {
        let mut live_ranges = HashMap::new();
        let mut instr_index = 0;
        
        for block in function.get_blocks() {
            for instr in block.get_instructions() {
                let used_temps = Self::get_instruction_temps(instr);
                
                for temp in used_temps {
                    live_ranges.entry(temp)
                        .and_modify(|(_first, last)| {
                            *last = instr_index;
                        })
                        .or_insert((instr_index, instr_index));
                }
                
                instr_index += 1;
            }
        }
        
        live_ranges
    }
    
    /// Get all temporaries used in a single instruction
    fn get_instruction_temps(instr: &IR) -> Vec<Temp> {
        match instr {
            IR::Binary { dest, left, right, .. } => vec![dest.clone(), left.clone(), right.clone()],
            IR::Unary { dest, operand, .. } => vec![dest.clone(), operand.clone()],
            IR::Load { dest, src, offset } => vec![dest.clone(), src.clone(), offset.clone()],
            IR::LoadImm { dest, .. } | IR::LoadFloat { dest, .. } => vec![dest.clone()],
            IR::CondBranch { reg1, reg2, .. } => vec![reg1.clone(), reg2.clone()],
            IR::Call { args, ret, .. } => {
                let mut temps = args.clone();
                if let Some(ret_temp) = ret {
                    temps.push(ret_temp.clone());
                }
                temps
            }
            IR::Return(Some(temp)) => vec![temp.clone()],
            IR::Alloc { dest, size } => vec![dest.clone(), size.clone()],
            IR::InitArray { base_addr, value, size } => {
                vec![base_addr.clone(), value.clone(), size.clone()]
            }
            _ => vec![],
        }
    }
}

/// Simple linear scan register allocator (can be extended/replaced)
pub struct LinearScanAllocator {
    available_regs: Vec<PhysicalRegister>,
    spill_count: usize,
}

impl LinearScanAllocator {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            available_regs: vec![
                // Use caller-saved registers first for temporaries
                PhysicalRegister::X5, PhysicalRegister::X6, PhysicalRegister::X7, // t0-t2
                PhysicalRegister::X28, PhysicalRegister::X29, PhysicalRegister::X30, PhysicalRegister::X31, // t3-t6
                PhysicalRegister::X10, PhysicalRegister::X11, // a0-a1 (also used for args/return)
                PhysicalRegister::X12, PhysicalRegister::X13, // a2-a3
                PhysicalRegister::X14, PhysicalRegister::X15, // a4-a5
                PhysicalRegister::X16, PhysicalRegister::X17, // a6-a7
                // Add callee-saved if needed
                PhysicalRegister::X9, PhysicalRegister::X18, PhysicalRegister::X19, // s1-s3
                PhysicalRegister::X20, PhysicalRegister::X21, PhysicalRegister::X22, // s4-s6
                PhysicalRegister::X23, PhysicalRegister::X24, PhysicalRegister::X25, // s7-s9
                PhysicalRegister::X26, PhysicalRegister::X27, // s10-s11
            ],
            spill_count: 0,
        }
    }
}

impl RegisterAllocator for LinearScanAllocator {
    fn allocate_registers(&mut self, function: &Function) -> HashMap<Temp, PhysicalRegister> {
        let mut allocation = HashMap::new();
        let live_ranges = IRAnalyzer::calculate_live_ranges(function);
        
        // Sort temporaries by their first use (simple linear scan)
        let mut sorted_temps: Vec<_> = live_ranges.iter().collect();
        sorted_temps.sort_by_key(|(_, (first_use, _))| *first_use);
        
        let mut reg_index = 0;
        
        for (temp, _) in sorted_temps {
            if reg_index < self.available_regs.len() {
                allocation.insert(temp.clone(), self.available_regs[reg_index].clone());
                reg_index += 1;
            } else {
                // Spill to stack
                allocation.insert(temp.clone(), PhysicalRegister::Spill(self.spill_count));
                self.spill_count += 1;
            }
        }
        
        allocation
    }
    
    fn get_spill_slots(&self) -> usize {
        self.spill_count
    }
}

/// Print utilities for debugging IR
#[allow(dead_code)]
pub struct IRPrinter;

impl IRPrinter {
    /// Print the entire IR in a readable format
    #[allow(dead_code)]
    pub fn print_ir(inter: &super::Inter) {
        println!("=== IR Representation ===");
        
        // Print functions
        for (label, function) in &inter.functions {
            println!("\nFunction: {}", label.get_name());
            println!("Parameters: {:?}", function.get_params());
        }
        
        // Print basic blocks
        for (i, block) in inter.basic_blocks.iter().enumerate() {
            println!("\nBasic Block {}: {}", i, block.get_label().get_name());
            println!("Predecessors: {:?}", block.get_predecessors().iter().map(|l| l.get_name()).collect::<Vec<_>>());
            println!("Successors: {:?}", block.get_successors().iter().map(|l| l.get_name()).collect::<Vec<_>>());
            
            for (j, instr) in block.get_instructions().iter().enumerate() {
                println!("  {}: {:?}", j, instr);
            }
        }
    }
    
    /// Print a single basic block
    #[allow(dead_code)]
    pub fn print_basic_block(block: &BasicBlock) {
        println!("Basic Block: {}", block.get_label().get_name());
        println!("Predecessors: {:?}", block.get_predecessors().iter().map(|l| l.get_name()).collect::<Vec<_>>());
        println!("Successors: {:?}", block.get_successors().iter().map(|l| l.get_name()).collect::<Vec<_>>());
        
        for (i, instr) in block.get_instructions().iter().enumerate() {
            println!("  {}: {:?}", i, instr);
        }
    }
    
    /// Print register allocation results
    #[allow(dead_code)]
    pub fn print_register_allocation(allocation: &HashMap<Temp, PhysicalRegister>) {
        println!("=== Register Allocation ===");
        for (temp, reg) in allocation {
            match reg {
                PhysicalRegister::Spill(slot) => {
                    println!("t{} -> spill slot {}", temp.get_id(), slot);
                }
                _ => {
                    println!("t{} -> {}", temp.get_id(), reg.to_asm_name());
                }
            }
        }
    }
}