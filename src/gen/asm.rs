/*
   probably future idea but general load/add/instr interfaces with a machine-specific backend
   would be nice for a future compiler or to implement in future
   since most RISC architectures have similar asm instructions
*/
pub mod instr {
    use crate::gen::asm::{allocate_register, free_register};

    pub fn load_imm(value: i32) -> (String, usize) {
        let reg = allocate_register() + 1;
        let instr = format!("\tli\tx{}, {}\n", reg + 1, value);
        return (instr, reg);
    }

    pub fn add(reg1: i32, reg2: i32) -> String {
        let instr = format!("\tadd\tx{reg1}, x{reg1}, x{reg2}\n");
        free_register(reg2 as usize);
        return instr;
    }

    pub fn sub(reg1: i32, reg2: i32) -> String {
        let instr = format!("\tsub\tx{reg1}, x{reg1}, x{reg2}\n");
        free_register(reg2 as usize);
        return instr;
    }

    pub fn mul(reg1: i32, reg2: i32) -> String {
        let instr = format!("\tmul\tx{reg1}, x{reg1}, x{reg2}\n");
        free_register(reg2 as usize);
        return instr;
    }

    pub fn div(reg1: i32, reg2: i32) -> String {
        let instr = format!("\tdiv\tx{reg1}, x{reg1}, x{reg2}\n");
        free_register(reg2 as usize);
        return instr;
    }
}

static mut REGISTER_ALLOC: [bool; 31] = [false; 31]; // true if allocated, false if not
fn allocate_register() -> usize {
    // gets first available register
    unsafe {
        for i in 0..REGISTER_ALLOC.len() {
            if !REGISTER_ALLOC[i] {
                REGISTER_ALLOC[i] = true;
                return i;
            }
        }
        eprintln!("ERR: register allocation error. Exiting");
        std::process::exit(-1);
    }
}

fn free_register(to_free: usize) {
    // returns a register back to the available pool
    unsafe {
        if !REGISTER_ALLOC[to_free - 1] {
            eprintln!("ERR: register free error. Exiting");
            std::process::exit(-1);
        } else {
            REGISTER_ALLOC[to_free - 1] = false;
        }
    }
}

pub fn file_start(file: &mut String) {
    let start = ".option\trvc\n\
        .section\t.text\n\
        .global\t_start\n\
        \n\
        _start:\n\
        \tli\tsp, 0x80000000\n\
        \tli\tgp, 0x80000000\n\
        \tli\tra, 0x0\n\
        \tj\tmain\n\
        \n\
        main:\n";
    file.push_str(start);
}
