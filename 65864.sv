module cpu65864 (
    input logic clk,
    input logic reset,
    input logic [63:0] data_in,
    output logic [63:0] data_out,
    output logic [63:0] addr,
    output logic rw,
    input logic irq,
    input logic nmi,
    input logic abrt,
    input logic ready,
    output logic sync,
    output logic [2:0] width,
    input logic vpa,
    input logic vda,
    output logic e,
    input logic m,
    input logic x
);

typedef enum logic [7:0] {
    // 6502/65C02 Instructions
    ADC_IMM = 8'h69, ADC_ZPG = 8'h65, ADC_ZPX = 8'h75, ADC_ABS = 8'h6D,
    ADC_ABX = 8'h7D, ADC_ABY = 8'h79, ADC_INX = 8'h61, ADC_INY = 8'h71,
    AND_IMM = 8'h29, AND_ZPG = 8'h25, AND_ZPX = 8'h35, AND_ABS = 8'h2D,
    AND_ABX = 8'h3D, AND_ABY = 8'h39, AND_INX = 8'h21, AND_INY = 8'h31,
    ASL_A = 8'h0A, ASL_ZPG = 8'h06, ASL_ZPX = 8'h16, ASL_ABS = 8'h0E,
    ASL_ABX = 8'h1E, BCC_REL = 8'h90, BCS_REL = 8'hB0, BEQ_REL = 8'hF0,
    BIT_ZPG = 8'h24, BIT_ABS = 8'h2C, BMI_REL = 8'h30, BNE_REL = 8'hD0,
    BPL_REL = 8'h10, BRK = 8'h00, BVC_REL = 8'h50, BVS_REL = 8'h70,
    CLC = 8'h18, CLD = 8'hD8, CLI = 8'h58, CLV = 8'hB8,
    CMP_IMM = 8'hC9, CMP_ZPG = 8'hC5, CMP_ZPX = 8'hD5, CMP_ABS = 8'hCD,
    CMP_ABX = 8'hDD, CMP_ABY = 8'hD9, CMP_INX = 8'hC1, CMP_INY = 8'hD1,
    CPX_IMM = 8'hE0, CPX_ZPG = 8'hE4, CPX_ABS = 8'hEC, CPY_IMM = 8'hC0,
    CPY_ZPG = 8'hC4, CPY_ABS = 8'hCC, DEC_A = 8'h3A, DEC_ZPG = 8'hC6,
    DEC_ZPX = 8'hD6, DEC_ABS = 8'hCE, DEC_ABX = 8'hDE, DEX = 8'hCA,
    DEY = 8'h88, EOR_IMM = 8'h49, EOR_ZPG = 8'h45, EOR_ZPX = 8'h55,
    EOR_ABS = 8'h4D, EOR_ABX = 8'h5D, EOR_ABY = 8'h59, EOR_INX = 8'h41,
    EOR_INY = 8'h51, INC_A = 8'h1A, INC_ZPG = 8'hE6, INC_ZPX = 8'hF6,
    INC_ABS = 8'hEE, INC_ABX = 8'hFE, INX = 8'hE8, INY = 8'hC8,
    JMP_ABS = 8'h4C, JMP_IND = 8'h6C, JSR_ABS = 8'h20, LDA_IMM = 8'hA9,
    LDA_ZPG = 8'hA5, LDA_ZPX = 8'hB5, LDA_ABS = 8'hAD, LDA_ABX = 8'hBD,
    LDA_ABY = 8'hB9, LDA_INX = 8'hA1, LDA_INY = 8'hB1, LDX_IMM = 8'hA2,
    LDX_ZPG = 8'hA6, LDX_ZPY = 8'hB6, LDX_ABS = 8'hAE, LDX_ABY = 8'hBE,
    LDY_IMM = 8'hA0, LDY_ZPG = 8'hA4, LDY_ZPX = 8'hB4, LDY_ABS = 8'hAC,
    LDY_ABX = 8'hBC, LSR_A = 8'h4A, LSR_ZPG = 8'h46, LSR_ZPX = 8'h56,
    LSR_ABS = 8'h4E, LSR_ABX = 8'h5E, NOP = 8'hEA, ORA_IMM = 8'h09,
    ORA_ZPG = 8'h05, ORA_ZPX = 8'h15, ORA_ABS = 8'h0D, ORA_ABX = 8'h1D,
    ORA_ABY = 8'h19, ORA_INX = 8'h01, ORA_INY = 8'h11, PHA = 8'h48,
    PHP = 8'h08, PLA = 8'h68, PLP = 8'h28, ROL_A = 8'h2A,
    ROL_ZPG = 8'h26, ROL_ZPX = 8'h36, ROL_ABS = 8'h2E, ROL_ABX = 8'h3E,
    ROR_A = 8'h6A, ROR_ZPG = 8'h66, ROR_ZPX = 8'h76, ROR_ABS = 8'h6E,
    ROR_ABX = 8'h7E, RTI = 8'h40, RTS = 8'h60, SBC_IMM = 8'hE9,
    SBC_ZPG = 8'hE5, SBC_ZPX = 8'hF5, SBC_ABS = 8'hED, SBC_ABX = 8'hFD,
    SBC_ABY = 8'hF9, SBC_INX = 8'hE1, SBC_INY = 8'hF1, SEC = 8'h38,
    SED = 8'hF8, SEI = 8'h78, STA_ZPG = 8'h85, STA_ZPX = 8'h95,
    STA_ABS = 8'h8D, STA_ABX = 8'h9D, STA_ABY = 8'h99, STA_INX = 8'h81,
    STA_INY = 8'h91, STX_ZPG = 8'h86, STX_ZPY = 8'h96, STX_ABS = 8'h8E,
    STY_ZPG = 8'h84, STY_ZPX = 8'h94, STY_ABS = 8'h8C, TAX = 8'hAA,
    TAY = 8'hA8, TSX = 8'hBA, TXA = 8'h8A, TXS = 8'h9A, TYA = 8'h98,
    
    // 65C02 Instructions
    ADC_IND = 8'h72, ADC_STK = 8'h63, ADC_STKY = 8'h73, AND_IND = 8'h32,
    AND_STK = 8'h23, AND_STKY = 8'h33, BIT_IMM = 8'h89, BIT_ZPX = 8'h34,
    BIT_ABX = 8'h3C, BRA_REL = 8'h80, CMP_IND = 8'hD2, CMP_STK = 8'hC3,
    CMP_STKY = 8'hD3, DEC_ACC = 8'h3A, EOR_IND = 8'h52, EOR_STK = 8'h43,
    EOR_STKY = 8'h53, INC_ACC = 8'h1A, JMP_INDX = 8'h7C, LDA_IND = 8'hB2,
    LDA_STK = 8'hA3, LDA_STKY = 8'hB3, ORA_IND = 8'h12, ORA_STK = 8'h03,
    ORA_STKY = 8'h13, PHX = 8'hDA, PHY = 8'h5A, PLX = 8'hFA,
    PLY = 8'h7A, SBC_IND = 8'hF2, SBC_STK = 8'hE3, SBC_STKY = 8'hF3,
    STZ_ZPG = 8'h64, STZ_ZPX = 8'h74, STZ_ABS = 8'h9C, STZ_ABX = 8'h9E,
    TRB_ZPG = 8'h14, TRB_ABS = 8'h1C, TSB_ZPG = 8'h04, TSB_ABS = 8'h0C,
    
    // 65816/65832 Instructions
    ADC_LONG = 8'h6F, ADC_LONGX = 8'h7F, ADC_INDY_L = 8'h77, ADC_SR = 8'h63,
    ADC_SRY = 8'h73, AND_LONG = 8'h2F, AND_LONGX = 8'h3F, AND_INDY_L = 8'h37,
    AND_SR = 8'h23, AND_SRY = 8'h33, BRL_REL = 8'h82, COP = 8'h02,
    CMP_LONG = 8'hCF, CMP_LONGX = 8'hDF, CMP_INDY_L = 8'hD7, CMP_SR = 8'hC3,
    CMP_SRY = 8'hD3, EOR_LONG = 8'h4F, EOR_LONGX = 8'h5F, EOR_INDY_L = 8'h57,
    EOR_SR = 8'h43, EOR_SRY = 8'h53, JMP_LONG = 8'h5C, JSR_LONG = 8'h22,
    JSR_INDX = 8'hFC, LDA_LONG = 8'hAF, LDA_LONGX = 8'hBF, LDA_INDY_L = 8'hB7,
    LDA_SR = 8'hA3, LDA_SRY = 8'hB3, LDX_LONG = 8'hAE, LDX_LONGY = 8'hBE,
    LDY_LONG = 8'hAC, LDY_LONGX = 8'hBC, MVN = 8'h54, MVP = 8'h44,
    ORA_LONG = 8'h0F, ORA_LONGX = 8'h1F, ORA_INDY_L = 8'h17, ORA_SR = 8'h03,
    ORA_SRY = 8'h13, PEA = 8'hF4, PEI = 8'hD4, PER = 8'h62,
    PHB = 8'h8B, PHD = 8'h0B, PHK = 8'h4B, PLB = 8'hAB,
    PLD = 8'h2B, REP = 8'hC2, RTL = 8'h6B, SBC_LONG = 8'hEF,
    SBC_LONGX = 8'hFF, SBC_INDY_L = 8'hF7, SBC_SR = 8'hE3, SBC_SRY = 8'hF3,
    SEP = 8'hE2, STA_LONG = 8'h8F, STA_LONGX = 8'h9F, STA_INDY_L = 8'h97,
    STA_SR = 8'h83, STA_SRY = 8'h93, STX_LONG = 8'h8E, STY_LONG = 8'h8C,
    TCD = 8'h5B, TCS = 8'h1B, TDC = 8'h7B, TSC = 8'h3B,
    TXY = 8'h9B, TYX = 8'hBB, WAI = 8'hCB, WDM = 8'h42,
    XBA = 8'hEB, XCE = 8'hFB,
    
    // 65864 Extended Instructions
    ADC_Q = 8'h6B, ADC_QX = 8'h7B, ADC_QY = 8'h79, ADC_INDZ = 8'h67,
    ADC_INDZY = 8'h77, ADD_IMM = 8'h69, ADD_Q = 8'h6D, ADD_QX = 8'h7D,
    AND_Q = 8'h2D, AND_QX = 8'h3D, ASL_Q = 8'h0E, ASL_QX = 8'h1E,
    BIT_Q = 8'h2C, BIT_QX = 8'h3C, CMP_Q = 8'hCD, CMP_QX = 8'hDD,
    CPX_Q = 8'hEC, CPY_Q = 8'hCC, DEC_Q = 8'hCE, DEC_QX = 8'hDE,
    EOR_Q = 8'h4D, EOR_QX = 8'h5D, INC_Q = 8'hEE, INC_QX = 8'hFE,
    LDA_Q = 8'hAD, LDA_QX = 8'hBD, LDX_Q = 8'hAE, LDX_QY = 8'hBE,
    LDY_Q = 8'hAC, LDY_QX = 8'hBC, LSR_Q = 8'h4E, LSR_QX = 8'h5E,
    ORA_Q = 8'h0D, ORA_QX = 8'h1D, ROL_Q = 8'h2E, ROL_QX = 8'h3E,
    ROR_Q = 8'h6E, ROR_QX = 8'h7E, SBC_Q = 8'hED, SBC_QX = 8'hFD,
    STA_Q = 8'h8D, STA_QX = 8'h9D, STX_Q = 8'h8E, STY_Q = 8'h8C,
    STZ_Q = 8'h9C, STZ_QX = 8'h9E, TRB_Q = 8'h1C, TSB_Q = 8'h0C,
    
    // 64-bit Specific Instructions
    PUSHQ = 8'h48, POPQ = 8'h68, MOVQ = 8'h89, CMPQ = 8'h39,
    ADDQ = 8'h03, SUBQ = 8'h2B, MULQ = 8'hF7, DIVQ = 8'hF6,
    SHLQ = 8'hD3, SHRQ = 8'hD3, ROLQ = 8'hC1, RORQ = 8'hC1,
    ANDQ = 8'h23, ORQ = 8'h0B, XORQ = 8'h33, NOTQ = 8'hF7,
    NEGQ = 8'hF7, INCQ = 8'hFF, DECQ = 8'hFF, TESTQ = 8'h85,
    LEAQ = 8'h8D, MOVSQ = 8'hA5, CMPSQ = 8'hA7, SCASQ = 8'hAF,
    LODSQ = 8'hAD, STOSQ = 8'hAB, XCHGQ = 8'h87, BSWAPQ = 8'h0F,
    BT_Q = 8'h0F, BTS_Q = 8'h0F, BTR_Q = 8'h0F, BTC_Q = 8'h0F,
    BSFQ = 8'h0F, BSRQ = 8'h0F, POPCNTQ = 8'hF3, TZCNTQ = 8'hF3,
    LZCNTQ = 8'hF3
} opcode_t;

typedef enum logic [3:0] {
    FETCH,
    DECODE,
    EXEC1,
    EXEC2,
    EXEC3,
    EXEC4,
    EXEC5,
    EXEC6,
    EXEC7,
    EXEC8,
    STORE1,
    STORE2,
    STORE3,
    STACK1,
    STACK2,
    STACK3,
    INTERRUPT,
    WAIT,
    HALT
} state_t;

// 64-bit Registers
logic [63:0] A;      // Accumulator
logic [63:0] X;      // Index X
logic [63:0] Y;      // Index Y
logic [63:0] S;      // Stack Pointer
logic [63:0] D;      // Direct Page
logic [63:0] B;      // Data Bank
logic [63:0] PC;     // Program Counter
logic [63:0] K;      // Program Bank
logic [63:0] IR;     // Instruction Register
logic [63:0] ADR;    // Address Register
logic [63:0] TEMP;   // Temporary Register
logic [63:0] TEMP2;  // Second Temporary
logic [63:0] Q;      // 64-bit temporary (Q register)
logic [63:0] DB;     // Data Bank (64-bit)

// Control Registers
logic [15:0] P;      // Status Register
logic [7:0] opcode;
state_t state;
logic [3:0] cycle;
logic nmi_latch;
logic irq_latch;
logic abrt_latch;
logic emulation;
logic native;
logic width_64;
logic m_flag;
logic x_flag;
logic e_flag;
logic [2:0] size_mode;
logic long_mode;
logic protected_mode;
logic [63:0] base_addr;
logic [63:0] limit_addr;

// Status Flags
wire N = P[15];
wire V = P[14];
wire M = P[13];
wire X = P[12];
wire D = P[11];
wire I = P[10];
wire Z = P[9];
wire C = P[8];
wire E = emulation;

// Processor Modes
typedef enum logic [1:0] {
    MODE_6502,
    MODE_65C02,
    MODE_65816,
    MODE_65864
} cpu_mode_t;

cpu_mode_t cpu_mode;

// Addressing Modes
typedef enum logic [3:0] {
    IMP, IMM, ZPG, ZPX, ZPY, ABS, ABX, ABY, IND, INX, INY, 
    INDX, INDY, SRI, SRY, LON, LONX, LONY, INDZ, INDZY,
    QAD, QAX, QAY, REL, RELL, ACC
} addr_mode_t;

addr_mode_t addr_mode;

// ALU Operations
typedef enum logic [3:0] {
    ALU_ADD, ALU_SUB, ALU_AND, ALU_OR, ALU_XOR, ALU_NOT, 
    ALU_NEG, ALU_INC, ALU_DEC, ALU_SHL, ALU_SHR, ALU_ROL, 
    ALU_ROR, ALU_MUL, ALU_DIV, ALU_CMP
} alu_op_t;

alu_op_t alu_op;

// 64-bit ALU
function automatic logic [63:0] alu64(
    input logic [63:0] a,
    input logic [63:0] b,
    input alu_op_t op,
    input logic cin,
    output logic cout,
    output logic overflow,
    output logic zero,
    output logic negative
);
    logic [63:0] result;
    logic [64:0] temp;
    
    case (op)
        ALU_ADD: begin
            temp = {1'b0, a} + {1'b0, b} + {63'b0, cin};
            result = temp[63:0];
            cout = temp[64];
            overflow = (a[63] == b[63]) && (result[63] != a[63]);
        end
        ALU_SUB: begin
            temp = {1'b0, a} - {1'b0, b} - {63'b0, ~cin};
            result = temp[63:0];
            cout = ~temp[64];
            overflow = (a[63] != b[63]) && (result[63] != a[63]);
        end
        ALU_AND: begin
            result = a & b;
            cout = 1'b0;
            overflow = 1'b0;
        end
        ALU_OR: begin
            result = a | b;
            cout = 1'b0;
            overflow = 1'b0;
        end
        ALU_XOR: begin
            result = a ^ b;
            cout = 1'b0;
            overflow = 1'b0;
        end
        ALU_NOT: begin
            result = ~a;
            cout = 1'b0;
            overflow = 1'b0;
        end
        ALU_NEG: begin
            result = ~a + 64'h1;
            cout = 1'b0;
            overflow = (a == 64'h8000000000000000);
        end
        ALU_INC: begin
            result = a + 64'h1;
            cout = (a == 64'hFFFFFFFFFFFFFFFF);
            overflow = (a == 64'h7FFFFFFFFFFFFFFF);
        end
        ALU_DEC: begin
            result = a - 64'h1;
            cout = (a == 64'h0);
            overflow = (a == 64'h8000000000000000);
        end
        ALU_SHL: begin
            result = a << b[5:0];
            cout = a[64 - b[5:0]];
            overflow = 1'b0;
        end
        ALU_SHR: begin
            result = a >> b[5:0];
            cout = a[b[5:0] - 1];
            overflow = 1'b0;
        end
        ALU_ROL: begin
            result = (a << b[5:0]) | (a >> (64 - b[5:0]));
            cout = a[64 - b[5:0]];
            overflow = 1'b0;
        end
        ALU_ROR: begin
            result = (a >> b[5:0]) | (a << (64 - b[5:0]));
            cout = a[b[5:0] - 1];
            overflow = 1'b0;
        end
        ALU_MUL: begin
            result = a * b;
            cout = 1'b0;
            overflow = (result[127:64] != 64'h0);
        end
        ALU_DIV: begin
            result = a / b;
            cout = 1'b0;
            overflow = (b == 64'h0);
        end
        ALU_CMP: begin
            result = a - b;
            cout = (a >= b);
            overflow = (a[63] != b[63]) && (result[63] != a[63]);
        end
        default: begin
            result = 64'h0;
            cout = 1'b0;
            overflow = 1'b0;
        end
    endcase
    
    zero = (result == 64'h0);
    negative = result[63];
    return result;
endfunction

// Instruction Decoder
function automatic addr_mode_t decode_addr_mode(input logic [7:0] op);
    case (op)
        // Immediate
        ADC_IMM, AND_IMM, CMP_IMM, CPX_IMM, CPY_IMM, EOR_IMM, 
        LDA_IMM, LDX_IMM, LDY_IMM, ORA_IMM, SBC_IMM, BIT_IMM,
        ADD_IMM: return IMM;
        
        // Zero Page
        ADC_ZPG, AND_ZPG, ASL_ZPG, BIT_ZPG, CMP_ZPG, CPX_ZPG,
        CPY_ZPG, DEC_ZPG, EOR_ZPG, INC_ZPG, LDA_ZPG, LDX_ZPG,
        LDY_ZPG, LSR_ZPG, ORA_ZPG, ROL_ZPG, ROR_ZPG, SBC_ZPG,
        STA_ZPG, STX_ZPG, STY_ZPG, STZ_ZPG, TRB_ZPG, TSB_ZPG: return ZPG;
        
        // Zero Page,X
        ADC_ZPX, AND_ZPX, ASL_ZPX, BIT_ZPX, CMP_ZPX, DEC_ZPX,
        EOR_ZPX, INC_ZPX, LDA_ZPX, LDY_ZPX, LSR_ZPX, ORA_ZPX,
        ROL_ZPX, ROR_ZPX, SBC_ZPX, STA_ZPX, STY_ZPX, STZ_ZPX: return ZPX;
        
        // Zero Page,Y
        LDX_ZPY, STX_ZPY: return ZPY;
        
        // Absolute
        ADC_ABS, AND_ABS, ASL_ABS, BIT_ABS, CMP_ABS, CPX_ABS,
        CPY_ABS, DEC_ABS, EOR_ABS, INC_ABS, JMP_ABS, JSR_ABS,
        LDA_ABS, LDX_ABS, LDY_ABS, LSR_ABS, ORA_ABS, ROL_ABS,
        ROR_ABS, SBC_ABS, STA_ABS, STX_ABS, STY_ABS, STZ_ABS,
        TRB_ABS, TSB_ABS: return ABS;
        
        // Absolute,X
        ADC_ABX, AND_ABX, ASL_ABX, BIT_ABX, CMP_ABX, DEC_ABX,
        EOR_ABX, INC_ABX, LDA_ABX, LDY_ABX, LSR_ABX, ORA_ABX,
        ROL_ABX, ROR_ABX, SBC_ABX, STA_ABX, STY_ABX, STZ_ABX: return ABX;
        
        // Absolute,Y
        ADC_ABY, AND_ABY, CMP_ABY, EOR_ABY, LDA_ABY, LDX_ABY,
        ORA_ABY, SBC_ABY, STA_ABY: return ABY;
        
        // Indirect
        JMP_IND: return IND;
        
        // (Indirect,X)
        ADC_INX, AND_INX, CMP_INX, EOR_INX, LDA_INX, ORA_INX,
        SBC_INX, STA_INX: return INX;
        
        // (Indirect),Y
        ADC_INY, AND_INY, CMP_INY, EOR_INY, LDA_INY, ORA_INY,
        SBC_INY, STA_INY: return INY;
        
        // [Indirect]
        ADC_IND, AND_IND, CMP_IND, EOR_IND, LDA_IND, ORA_IND,
        SBC_IND, STA_IND: return INDX;
        
        // [Indirect],Y
        ADC_INDY_L, AND_INDY_L, CMP_INDY_L, EOR_INDY_L, LDA_INDY_L,
        ORA_INDY_L, SBC_INDY_L, STA_INDY_L: return INDY;
        
        // (Stack,S),Y
        ADC_SRY, AND_SRY, CMP_SRY, EOR_SRY, LDA_SRY, ORA_SRY,
        SBC_SRY, STA_SRY: return SRY;
        
        // Long
        ADC_LONG, AND_LONG, CMP_LONG, EOR_LONG, LDA_LONG,
        ORA_LONG, SBC_LONG, STA_LONG: return LON;
        
        // Long,X
        ADC_LONGX, AND_LONGX, CMP_LONGX, EOR_LONGX, LDA_LONGX,
        ORA_LONGX, SBC_LONGX, STA_LONGX: return LONX;
        
        // Long,Y
        LDX_LONGY, LDY_LONGX: return LONY;
        
        // 64-bit Quad
        ADC_Q, AND_Q, CMP_Q, EOR_Q, LDA_Q, ORA_Q, SBC_Q,
        STA_Q, STX_Q, STY_Q, STZ_Q, TRB_Q, TSB_Q: return QAD;
        
        // 64-bit Quad,X
        ADC_QX, AND_QX, CMP_QX, EOR_QX, LDA_QX, ORA_QX,
        SBC_QX, STA_QX, STZ_QX: return QAX;
        
        // 64-bit Quad,Y
        ADC_QY, LDX_QY, LDY_QX: return QAY;
        
        // Relative
        BCC_REL, BCS_REL, BEQ_REL, BMI_REL, BNE_REL, BPL_REL,
        BVC_REL, BVS_REL, BRA_REL: return REL;
        
        // Long Relative
        BRL_REL: return RELL;
        
        // Accumulator
        ASL_A, DEC_A, INC_A, LSR_A, ROL_A, ROR_A: return ACC;
        
        // Implied
        default: return IMP;
    endcase
endfunction

// Calculate Effective Address
function automatic logic [63:0] calc_ea(
    input addr_mode_t mode,
    input logic [63:0] pc_val,
    input logic [63:0] d_val,
    input logic [63:0] x_val,
    input logic [63:0] y_val,
    input logic [63:0] s_val,
    input logic [63:0] db_val,
    input logic [63:0] k_val
);
    logic [63:0] ea;
    case (mode)
        IMM: ea = pc_val;
        ZPG: ea = {56'h0, data_in[7:0]};
        ZPX: ea = {56'h0, data_in[7:0]} + x_val[7:0];
        ZPY: ea = {56'h0, data_in[7:0]} + y_val[7:0];
        ABS: ea = {db_val[55:0], 48'h0, data_in[15:0]};
        ABX: ea = {db_val[55:0], 48'h0, data_in[15:0]} + x_val;
        ABY: ea = {db_val[55:0], 48'h0, data_in[15:0]} + y_val;
        IND: ea = {db_val[55:0], 48'h0, data_in[15:0]};
        INX: ea = {56'h0, data_in[7:0]} + x_val[7:0];
        INY: begin
            ea = {56'h0, data_in[7:0]};
            ea = {db_val[55:0], 48'h0, ea[15:0]} + y_val;
        end
        INDX: ea = {db_val[55:0], 48'h0, data_in[15:0]};
        INDY: begin
            ea = {db_val[55:0], 48'h0, data_in[15:0]};
            ea = ea + y_val;
        end
        SRI: ea = s_val + {56'h0, data_in[7:0]};
        SRY: ea = s_val + {56'h0, data_in[7:0]} + y_val[7:0];
        LON: ea = {40'h0, data_in[23:0]};
        LONX: ea = {40'h0, data_in[23:0]} + x_val;
        LONY: ea = {40'h0, data_in[23:0]} + y_val;
        QAD: ea = data_in[63:0];
        QAX: ea = data_in[63:0] + x_val;
        QAY: ea = data_in[63:0] + y_val;
        REL: ea = pc_val + {{56{data_in[7]}}, data_in[7:0]};
        RELL: ea = pc_val + {{48{data_in[15]}}, data_in[15:0]};
        default: ea = 64'h0;
    endcase
    return ea;
endfunction

// Main State Machine
always_ff @(posedge clk) begin
    if (reset) begin
        // Reset all registers
        A <= 64'h0000000000000000;
        X <= 64'h0000000000000000;
        Y <= 64'h0000000000000000;
        S <= 64'h000000000001FF;
        D <= 64'h0000000000000000;
        B <= 64'h0000000000000000;
        DB <= 64'h0000000000000000;
        K <= 64'h0000000000000000;
        PC <= {56'h00000000000000, 16'hFFFC};
        P <= 16'h3000;
        state <= FETCH;
        cycle <= 4'b0000;
        rw <= 1'b1;
        sync <= 1'b0;
        nmi_latch <= 1'b0;
        irq_latch <= 1'b0;
        abrt_latch <= 1'b0;
        emulation <= 1'b1;
        native <= 1'b0;
        width_64 <= 1'b0;
        m_flag <= 1'b1;
        x_flag <= 1'b1;
        e_flag <= 1'b1;
        cpu_mode <= MODE_6502;
        long_mode <= 1'b0;
        protected_mode <= 1'b0;
        base_addr <= 64'h0;
        limit_addr <= 64'hFFFFFFFFFFFFFFFF;
        width <= 3'b000;
    end else if (ready) begin
        case (state)
            FETCH: begin
                sync <= 1'b1;
                addr <= {K[55:0], PC[63:0]};
                rw <= 1'b1;
                PC <= PC + (width_64 ? 64'h8 : (m_flag ? 64'h2 : 64'h1));
                state <= DECODE;
                width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
            end
            DECODE: begin
                sync <= 1'b0;
                opcode <= data_in[7:0];
                IR <= data_in;
                addr_mode <= decode_addr_mode(data_in[7:0]);
                cycle <= 4'b0000;
                
                // Check interrupts
                if (nmi && !nmi_latch) begin
                    state <= INTERRUPT;
                    nmi_latch <= 1'b1;
                end else if (irq && !I) begin
                    state <= INTERRUPT;
                    irq_latch <= 1'b1;
                end else if (abrt && !abrt_latch) begin
                    state <= INTERRUPT;
                    abrt_latch <= 1'b1;
                end else begin
                    state <= EXEC1;
                end
            end
            EXEC1: begin
                case (addr_mode)
                    IMM: begin
                        addr <= {K[55:0], PC[63:0]};
                        PC <= PC + (width_64 ? 64'h8 : (m_flag ? 64'h2 : 64'h1));
                        TEMP <= data_in;
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    ZPG, ZPX, ZPY: begin
                        addr <= PC;
                        PC <= PC + 64'h1;
                        state <= EXEC2;
                        width <= 3'b000;
                    end
                    ABS, ABX, ABY: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h2);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b001;
                    end
                    LON, LONX, LONY: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h3);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b010;
                    end
                    QAD, QAX, QAY: begin
                        addr <= PC;
                        PC <= PC + 64'h8;
                        state <= EXEC2;
                        width <= 3'b111;
                    end
                    IND, INX, INY: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h2);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b001;
                    end
                    INDX, INDY, SRI, SRY: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h2);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b001;
                    end
                    REL: begin
                        addr <= PC;
                        PC <= PC + 64'h1;
                        state <= EXEC2;
                        width <= 3'b000;
                    end
                    RELL: begin
                        addr <= PC;
                        PC <= PC + 64'h2;
                        state <= EXEC2;
                        width <= 3'b001;
                    end
                    ACC, IMP: begin
                        state <= EXEC2;
                    end
                    default: begin
                        state <= FETCH;
                    end
                endcase
            end
            EXEC2: begin
                // Calculate effective address
                ADR <= calc_ea(addr_mode, PC, D, X, Y, S, DB, K);
                
                // Determine operation
                case (opcode)
                    // Load Operations
                    LDA_IMM, LDA_ZPG, LDA_ZPX, LDA_ABS, LDA_ABX, LDA_ABY,
                    LDA_INX, LDA_INY, LDA_IND, LDA_SRY, LDA_LONG, LDA_LONGX,
                    LDA_Q, LDA_QX: begin
                        if (addr_mode == IMM) begin
                            state <= EXEC3;
                        end else begin
                            addr <= ADR;
                            state <= EXEC3;
                        end
                    end
                    
                    // Store Operations
                    STA_ZPG, STA_ZPX, STA_ABS, STA_ABX, STA_ABY, STA_INX,
                    STA_INY, STA_IND, STA_SRY, STA_LONG, STA_LONGX,
                    STA_Q, STA_QX: begin
                        data_out <= A;
                        rw <= 1'b0;
                        addr <= ADR;
                        state <= FETCH;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    
                    // Arithmetic Operations
                    ADC_IMM, ADC_ZPG, ADC_ZPX, ADC_ABS, ADC_ABX, ADC_ABY,
                    ADC_INX, ADC_INY, ADC_IND, ADC_SRY, ADC_LONG, ADC_LONGX,
                    ADC_Q, ADC_QX: begin
                        if (addr_mode == IMM) begin
                            alu_op <= ALU_ADD;
                            state <= EXEC3;
                        end else begin
                            addr <= ADR;
                            state <= EXEC3;
                        end
                    end
                    
                    SBC_IMM, SBC_ZPG, SBC_ZPX, SBC_ABS, SBC_ABX, SBC_ABY,
                    SBC_INX, SBC_INY, SBC_IND, SBC_SRY, SBC_LONG, SBC_LONGX,
                    SBC_Q, SBC_QX: begin
                        if (addr_mode == IMM) begin
                            alu_op <= ALU_SUB;
                            state <= EXEC3;
                        end else begin
                            addr <= ADR;
                            state <= EXEC3;
                        end
                    end
                    
                    // Logic Operations
                    AND_IMM, AND_ZPG, AND_ZPX, AND_ABS, AND_ABX, AND_ABY,
                    AND_INX, AND_INY, AND_IND, AND_SRY, AND_LONG, AND_LONGX,
                    AND_Q, AND_QX: begin
                        if (addr_mode == IMM) begin
                            alu_op <= ALU_AND;
                            state <= EXEC3;
                        end else begin
                            addr <= ADR;
                            state <= EXEC3;
                        end
                    end
                    
                    ORA_IMM, ORA_ZPG, ORA_ZPX, ORA_ABS, ORA_ABX, ORA_ABY,
                    ORA_INX, ORA_INY, ORA_IND, ORA_SRY, ORA_LONG, ORA_LONGX,
                    ORA_Q, ORA_QX: begin
                        if (addr_mode == IMM) begin
                            alu_op <= ALU_OR;
                            state <= EXEC3;
                        end else begin
                            addr <= ADR;
                            state <= EXEC3;
                        end
                    end
                    
                    EOR_IMM, EOR_ZPG, EOR_ZPX, EOR_ABS, EOR_ABX, EOR_ABY,
                    EOR_INX, EOR_INY, EOR_IND, EOR_SRY, EOR_LONG, EOR_LONGX,
                    EOR_Q, EOR_QX: begin
                        if (addr_mode == IMM) begin
                            alu_op <= ALU_XOR;
                            state <= EXEC3;
                        end else begin
                            addr <= ADR;
                            state <= EXEC3;
                        end
                    end
                    
                    // Compare Operations
                    CMP_IMM, CMP_ZPG, CMP_ZPX, CMP_ABS, CMP_ABX, CMP_ABY,
                    CMP_INX, CMP_INY, CMP_IND, CMP_SRY, CMP_LONG, CMP_LONGX,
                    CMP_Q, CMP_QX: begin
                        if (addr_mode == IMM) begin
                            alu_op <= ALU_CMP;
                            state <= EXEC3;
                        end else begin
                            addr <= ADR;
                            state <= EXEC3;
                        end
                    end
                    
                    CPX_IMM, CPX_ZPG, CPX_ABS, CPX_Q: begin
                        alu_op <= ALU_CMP;
                        TEMP <= X;
                        if (addr_mode == IMM) begin
                            state <= EXEC3;
                        end else begin
                            addr <= ADR;
                            state <= EXEC3;
                        end
                    end
                    
                    CPY_IMM, CPY_ZPG, CPY_ABS, CPY_Q: begin
                        alu_op <= ALU_CMP;
                        TEMP <= Y;
                        if (addr_mode == IMM) begin
                            state <= EXEC3;
                        end else begin
                            addr <= ADR;
                            state <= EXEC3;
                        end
                    end
                    
                    // Shift/Rotate Operations
                    ASL_A, ROL_A, ROR_A, LSR_A: begin
                        case (opcode)
                            ASL_A: alu_op <= ALU_SHL;
                            ROL_A: alu_op <= ALU_ROL;
                            ROR_A: alu_op <= ALU_ROR;
                            LSR_A: alu_op <= ALU_SHR;
                        endcase
                        TEMP <= width_64 ? 64'h1 : (m_flag ? 16'h1 : 8'h1);
                        state <= EXEC3;
                    end
                    
                    // Branch Operations
                    BCC_REL, BCS_REL, BEQ_REL, BMI_REL, BNE_REL, BPL_REL,
                    BVC_REL, BVS_REL, BRA_REL: begin
                        logic take_branch;
                        case (opcode)
                            BCC_REL: take_branch = ~C;
                            BCS_REL: take_branch = C;
                            BEQ_REL: take_branch = Z;
                            BMI_REL: take_branch = N;
                            BNE_REL: take_branch = ~Z;
                            BPL_REL: take_branch = ~N;
                            BVC_REL: take_branch = ~V;
                            BVS_REL: take_branch = V;
                            BRA_REL: take_branch = 1'b1;
                            default: take_branch = 1'b0;
                        endcase
                        
                        if (take_branch) begin
                            PC <= ADR;
                        end
                        state <= FETCH;
                    end
                    
                    BRL_REL: begin
                        PC <= ADR;
                        state <= FETCH;
                    end
                    
                    // Jump Operations
                    JMP_ABS, JMP_IND, JMP_LONG: begin
                        PC <= ADR;
                        state <= FETCH;
                    end
                    
                    // JSR Operations
                    JSR_ABS, JSR_LONG: begin
                        addr <= S;
                        if (width_64) begin
                            data_out <= PC;
                        end else begin
                            data_out <= {48'h0, PC[15:0]};
                        end
                        rw <= 1'b0;
                        S <= S - (width_64 ? 64'h8 : 64'h2);
                        state <= STACK1;
                        width <= width_64 ? 3'b111 : 3'b001;
                    end
                    
                    // RTS/RTL
                    RTS: begin
                        S <= S + (width_64 ? 64'h8 : 64'h2);
                        addr <= S;
                        state <= EXEC3;
                        width <= width_64 ? 3'b111 : 3'b001;
                    end
                    
                    RTL: begin
                        S <= S + (width_64 ? 64'h8 : 64'h3);
                        addr <= S;
                        state <= EXEC3;
                        width <= width_64 ? 3'b111 : 3'b010;
                    end
                    
                    // Stack Operations
                    PHA: begin
                        addr <= S;
                        data_out <= A;
                        rw <= 1'b0;
                        S <= S - (width_64 ? 64'h8 : (m_flag ? 64'h2 : 64'h1));
                        state <= FETCH;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    
                    PLA: begin
                        S <= S + (width_64 ? 64'h8 : (m_flag ? 64'h2 : 64'h1));
                        addr <= S;
                        state <= EXEC3;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    
                    PHX, PHY: begin
                        addr <= S;
                        data_out <= (opcode == PHX) ? X : Y;
                        rw <= 1'b0;
                        S <= S - (width_64 ? 64'h8 : (m_flag ? 64'h2 : 64'h1));
                        state <= FETCH;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    
                    PLX, PLY: begin
                        S <= S + (width_64 ? 64'h8 : (m_flag ? 64'h2 : 64'h1));
                        addr <= S;
                        state <= EXEC3;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    
                    // Transfer Operations
                    TAX: begin
                        X <= A;
                        P[9] <= (A == 64'h0);
                        P[15] <= A[63];
                        state <= FETCH;
                    end
                    
                    TAY: begin
                        Y <= A;
                        P[9] <= (A == 64'h0);
                        P[15] <= A[63];
                        state <= FETCH;
                    end
                    
                    TXA: begin
                        A <= X;
                        P[9] <= (X == 64'h0);
                        P[15] <= X[63];
                        state <= FETCH;
                    end
                    
                    TYA: begin
                        A <= Y;
                        P[9] <= (Y == 64'h0);
                        P[15] <= Y[63];
                        state <= FETCH;
                    end
                    
                    TSX: begin
                        X <= S;
                        P[9] <= (S == 64'h0);
                        P[15] <= S[63];
                        state <= FETCH;
                    end
                    
                    TXS: begin
                        S <= X;
                        state <= FETCH;
                    end
                    
                    // Increment/Decrement
                    INX: begin
                        X <= X + (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1));
                        P[9] <= ((X + (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1))) == 64'h0);
                        P[15] <= (X + (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1)))[63];
                        state <= FETCH;
                    end
                    
                    INY: begin
                        Y <= Y + (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1));
                        P[9] <= ((Y + (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1))) == 64'h0);
                        P[15] <= (Y + (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1)))[63];
                        state <= FETCH;
                    end
                    
                    DEX: begin
                        X <= X - (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1));
                        P[9] <= ((X - (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1))) == 64'h0);
                        P[15] <= (X - (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1)))[63];
                        state <= FETCH;
                    end
                    
                    DEY: begin
                        Y <= Y - (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1));
                        P[9] <= ((Y - (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1))) == 64'h0);
                        P[15] <= (Y - (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1)))[63];
                        state <= FETCH;
                    end
                    
                    // Flag Operations
                    CLC: begin
                        P[8] <= 1'b0;
                        state <= FETCH;
                    end
                    
                    SEC: begin
                        P[8] <= 1'b1;
                        state <= FETCH;
                    end
                    
                    CLI: begin
                        P[10] <= 1'b0;
                        state <= FETCH;
                    end
                    
                    SEI: begin
                        P[10] <= 1'b1;
                        state <= FETCH;
                    end
                    
                    CLD: begin
                        P[11] <= 1'b0;
                        state <= FETCH;
                    end
                    
                    SED: begin
                        P[11] <= 1'b1;
                        state <= FETCH;
                    end
                    
                    CLV: begin
                        P[14] <= 1'b0;
                        state <= FETCH;
                    end
                    
                    // 65816/65832 Mode Operations
                    XCE: begin
                        e_flag <= C;
                        P[8] <= emulation;
                        emulation <= e_flag;
                        native <= ~e_flag;
                        state <= FETCH;
                    end
                    
                    REP: begin
                        P <= P & ~data_in[15:0];
                        m_flag <= ~data_in[13];
                        x_flag <= ~data_in[12];
                        state <= FETCH;
                    end
                    
                    SEP: begin
                        P <= P | data_in[15:0];
                        m_flag <= data_in[13];
                        x_flag <= data_in[12];
                        state <= FETCH;
                    end
                    
                    // 64-bit Specific Operations
                    ADDQ: begin
                        A <= alu64(A, TEMP, ALU_ADD, C, P[8], P[14], P[9], P[15]);
                        state <= FETCH;
                    end
                    
                    SUBQ: begin
                        A <= alu64(A, TEMP, ALU_SUB, C, P[8], P[14], P[9], P[15]);
                        state <= FETCH;
                    end
                    
                    MULQ: begin
                        TEMP2 <= alu64(A, TEMP, ALU_MUL, 1'b0, P[8], P[14], P[9], P[15]);
                        A <= TEMP2[63:0];
                        Q <= TEMP2[127:64];
                        state <= FETCH;
                    end
                    
                    DIVQ: begin
                        if (TEMP != 64'h0) begin
                            A <= alu64(A, TEMP, ALU_DIV, 1'b0, P[8], P[14], P[9], P[15]);
                            Q <= A % TEMP;
                        end
                        state <= FETCH;
                    end
                    
                    SHLQ: begin
                        A <= alu64(A, TEMP, ALU_SHL, 1'b0, P[8], P[14], P[9], P[15]);
                        state <= FETCH;
                    end
                    
                    SHRQ: begin
                        A <= alu64(A, TEMP, ALU_SHR, 1'b0, P[8], P[14], P[9], P[15]);
                        state <= FETCH;
                    end
                    
                    ROLQ: begin
                        A <= alu64(A, TEMP, ALU_ROL, 1'b0, P[8], P[14], P[9], P[15]);
                        state <= FETCH;
                    end
                    
                    RORQ: begin
                        A <= alu64(A, TEMP, ALU_ROR, 1'b0, P[8], P[14], P[9], P[15]);
                        state <= FETCH;
                    end
                    
                    ANDQ: begin
                        A <= alu64(A, TEMP, ALU_AND, 1'b0, P[8], P[14], P[9], P[15]);
                        state <= FETCH;
                    end
                    
                    ORQ: begin
                        A <= alu64(A, TEMP, ALU_OR, 1'b0, P[8], P[14], P[9], P[15]);
                        state <= FETCH;
                    end
                    
                    XORQ: begin
                        A <= alu64(A, TEMP, ALU_XOR, 1'b0, P[8], P[14], P[9], P[15]);
                        state <= FETCH;
                    end
                    
                    NOTQ: begin
                        A <= alu64(A, 64'h0, ALU_NOT, 1'b0, P[8], P[14], P[9], P[15]);
                        state <= FETCH;
                    end
                    
                    NEGQ: begin
                        A <= alu64(A, 64'h0, ALU_NEG, 1'b0, P[8], P[14], P[9], P[15]);
                        state <= FETCH;
                    end
                    
                    // Block Move Operations
                    MVN, MVP: begin
                        B <= {56'h0, data_in[15:8]};
                        DB <= {56'h0, data_in[7:0]};
                        state <= EXEC3;
                    end
                    
                    // Wait/Stop
                    WAI: begin
                        state <= WAIT;
                    end
                    
                    STP: begin
                        state <= HALT;
                    end
                    
                    // No Operation
                    NOP: begin
                        state <= FETCH;
                    end
                    
                    default: begin
                        state <= FETCH;
                    end
                endcase
            end
            EXEC3: begin
                case (opcode)
                    // Load Operations
                    LDA_IMM: begin
                        if (width_64) begin
                            A <= TEMP;
                            P[9] <= (TEMP == 64'h0);
                            P[15] <= TEMP[63];
                        end else if (m_flag) begin
                            A[15:0] <= TEMP[15:0];
                            P[9] <= (TEMP[15:0] == 16'h0);
                            P[15] <= TEMP[15];
                        end else begin
                            A[7:0] <= TEMP[7:0];
                            P[9] <= (TEMP[7:0] == 8'h0);
                            P[15] <= TEMP[7];
                        end
                        state <= FETCH;
                    end
                    
                    // Memory Read Operations
                    default: begin
                        if (addr_mode != IMM) begin
                            TEMP <= data_in;
                            state <= EXEC4;
                        end
                    end
                endcase
            end
            EXEC4: begin
                // Execute ALU operation
                logic cout, overflow, zero, negative;
                logic [63:0] alu_result;
                
                case (opcode)
                    ADC_IMM, ADC_ZPG, ADC_ZPX, ADC_ABS, ADC_ABX, ADC_ABY,
                    ADC_INX, ADC_INY, ADC_IND, ADC_SRY, ADC_LONG, ADC_LONGX,
                    ADC_Q, ADC_QX, ADDQ: begin
                        alu_result = alu64(A, TEMP, ALU_ADD, C, cout, overflow, zero, negative);
                        A <= alu_result;
                        P[8] <= cout;
                        P[14] <= overflow;
                        P[9] <= zero;
                        P[15] <= negative;
                    end
                    
                    SBC_IMM, SBC_ZPG, SBC_ZPX, SBC_ABS, SBC_ABX, SBC_ABY,
                    SBC_INX, SBC_INY, SBC_IND, SBC_SRY, SBC_LONG, SBC_LONGX,
                    SBC_Q, SBC_QX, SUBQ: begin
                        alu_result = alu64(A, TEMP, ALU_SUB, ~C, cout, overflow, zero, negative);
                        A <= alu_result;
                        P[8] <= cout;
                        P[14] <= overflow;
                        P[9] <= zero;
                        P[15] <= negative;
                    end
                    
                    AND_IMM, AND_ZPG, AND_ZPX, AND_ABS, AND_ABX, AND_ABY,
                    AND_INX, AND_INY, AND_IND, AND_SRY, AND_LONG, AND_LONGX,
                    AND_Q, AND_QX, ANDQ: begin
                        alu_result = alu64(A, TEMP, ALU_AND, 1'b0, cout, overflow, zero, negative);
                        A <= alu_result;
                        P[9] <= zero;
                        P[15] <= negative;
                    end
                    
                    ORA_IMM, ORA_ZPG, ORA_ZPX, ORA_ABS, ORA_ABX, ORA_ABY,
                    ORA_INX, ORA_INY, ORA_IND, ORA_SRY, ORA_LONG, ORA_LONGX,
                    ORA_Q, ORA_QX, ORQ: begin
                        alu_result = alu64(A, TEMP, ALU_OR, 1'b0, cout, overflow, zero, negative);
                        A <= alu_result;
                        P[9] <= zero;
                        P[15] <= negative;
                    end
                    
                    EOR_IMM, EOR_ZPG, EOR_ZPX, EOR_ABS, EOR_ABX, EOR_ABY,
                    EOR_INX, EOR_INY, EOR_IND, EOR_SRY, EOR_LONG, EOR_LONGX,
                    EOR_Q, EOR_QX, XORQ: begin
                        alu_result = alu64(A, TEMP, ALU_XOR, 1'b0, cout, overflow, zero, negative);
                        A <= alu_result;
                        P[9] <= zero;
                        P[15] <= negative;
                    end
                    
                    CMP_IMM, CMP_ZPG, CMP_ZPX, CMP_ABS, CMP_ABX, CMP_ABY,
                    CMP_INX, CMP_INY, CMP_IND, CMP_SRY, CMP_LONG, CMP_LONGX,
                    CMP_Q, CMP_QX, CMPQ: begin
                        alu_result = alu64(A, TEMP, ALU_CMP, 1'b0, cout, overflow, zero, negative);
                        P[8] <= cout;
                        P[14] <= overflow;
                        P[9] <= zero;
                        P[15] <= negative;
                    end
                    
                    CPX_IMM, CPX_ZPG, CPX_ABS, CPX_Q: begin
                        alu_result = alu64(X, TEMP, ALU_CMP, 1'b0, cout, overflow, zero, negative);
                        P[8] <= cout;
                        P[14] <= overflow;
                        P[9] <= zero;
                        P[15] <= negative;
                    end
                    
                    CPY_IMM, CPY_ZPG, CPY_ABS, CPY_Q: begin
                        alu_result = alu64(Y, TEMP, ALU_CMP, 1'b0, cout, overflow, zero, negative);
                        P[8] <= cout;
                        P[14] <= overflow;
                        P[9] <= zero;
                        P[15] <= negative;
                    end
                    
                    // Load from memory
                    LDA_ZPG, LDA_ZPX, LDA_ABS, LDA_ABX, LDA_ABY, LDA_INX,
                    LDA_INY, LDA_IND, LDA_SRY, LDA_LONG, LDA_LONGX,
                    LDA_Q, LDA_QX: begin
                        if (width_64) begin
                            A <= data_in;
                            P[9] <= (data_in == 64'h0);
                            P[15] <= data_in[63];
                        end else if (m_flag) begin
                            A[15:0] <= data_in[15:0];
                            P[9] <= (data_in[15:0] == 16'h0);
                            P[15] <= data_in[15];
                        end else begin
                            A[7:0] <= data_in[7:0];
                            P[9] <= (data_in[7:0] == 8'h0);
                            P[15] <= data_in[7];
                        end
                    end
                    
                    // Shift/Rotate
                    ASL_A, ROL_A, ROR_A, LSR_A: begin
                        case (opcode)
                            ASL_A: alu_result = alu64(A, TEMP, ALU_SHL, C, cout, overflow, zero, negative);
                            ROL_A: alu_result = alu64(A, TEMP, ALU_ROL, C, cout, overflow, zero, negative);
                            ROR_A: alu_result = alu64(A, TEMP, ALU_ROR, C, cout, overflow, zero, negative);
                            LSR_A: alu_result = alu64(A, TEMP, ALU_SHR, C, cout, overflow, zero, negative);
                        endcase
                        A <= alu_result;
                        P[8] <= cout;
                        P[9] <= zero;
                        P[15] <= negative;
                    end
                    
                    // Stack Operations
                    PLA: begin
                        if (width_64) begin
                            A <= data_in;
                            P[9] <= (data_in == 64'h0);
                            P[15] <= data_in[63];
                        end else if (m_flag) begin
                            A[15:0] <= data_in[15:0];
                            P[9] <= (data_in[15:0] == 16'h0);
                            P[15] <= data_in[15];
                        end else begin
                            A[7:0] <= data_in[7:0];
                            P[9] <= (data_in[7:0] == 8'h0);
                            P[15] <= data_in[7];
                        end
                    end
                    
                    PLX: begin
                        if (width_64) begin
                            X <= data_in;
                            P[9] <= (data_in == 64'h0);
                            P[15] <= data_in[63];
                        end else if (x_flag) begin
                            X[15:0] <= data_in[15:0];
                            P[9] <= (data_in[15:0] == 16'h0);
                            P[15] <= data_in[15];
                        end else begin
                            X[7:0] <= data_in[7:0];
                            P[9] <= (data_in[7:0] == 8'h0);
                            P[15] <= data_in[7];
                        end
                    end
                    
                    PLY: begin
                        if (width_64) begin
                            Y <= data_in;
                            P[9] <= (data_in == 64'h0);
                            P[15] <= data_in[63];
                        end else if (x_flag) begin
                            Y[15:0] <= data_in[15:0];
                            P[9] <= (data_in[15:0] == 16'h0);
                            P[15] <= data_in[15];
                        end else begin
                            Y[7:0] <= data_in[7:0];
                            P[9] <= (data_in[7:0] == 8'h0);
                            P[15] <= data_in[7];
                        end
                    end
                    
                    // Block Move Operations
                    MVN, MVP: begin
                        TEMP2 <= data_in;
                        addr <= {DB[55:0], Y[63:0]};
                        state <= EXEC5;
                    end
                endcase
                
                if (!(opcode inside {MVN, MVP})) begin
                    state <= FETCH;
                end
            end
            EXEC5: begin
                // Block Move Execution
                case (opcode)
                    MVN: begin
                        data_out <= TEMP2;
                        rw <= 1'b0;
                        addr <= {DB[55:0], Y[63:0]};
                        X <= X + (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1));
                        Y <= Y + (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1));
                        A <= A - (width_64 ? 64'h1 : (m_flag ? 16'h1 : 8'h1));
                        if (A != 64'h0) begin
                            addr <= {B[55:0], X[63:0]};
                            state <= EXEC4;
                        end else begin
                            state <= FETCH;
                        end
                    end
                    
                    MVP: begin
                        data_out <= TEMP2;
                        rw <= 1'b0;
                        addr <= {DB[55:0], Y[63:0]};
                        X <= X - (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1));
                        Y <= Y - (width_64 ? 64'h1 : (x_flag ? 16'h1 : 8'h1));
                        A <= A - (width_64 ? 64'h1 : (m_flag ? 16'h1 : 8'h1));
                        if (A != 64'h0) begin
                            addr <= {B[55:0], X[63:0]};
                            state <= EXEC4;
                        end else begin
                            state <= FETCH;
                        end
                    end
                endcase
            end
            STACK1: begin
                // Complete JSR
                if (opcode == JSR_ABS) begin
                    PC <= {K[55:0], ADR[63:0]};
                end else if (opcode == JSR_LONG) begin
                    PC <= ADR;
                end
                state <= FETCH;
            end
            INTERRUPT: begin
                case (cycle)
                    4'b0000: begin
                        // Push Program Bank and Counter
                        addr <= S;
                        if (emulation) begin
                            data_out <= {56'h0, PC[15:0]};
                        end else if (width_64) begin
                            data_out <= {K[55:0], PC[63:0]};
                        end else begin
                            data_out <= {K[55:0], 48'h0, PC[15:0]};
                        end
                        rw <= 1'b0;
                        S <= S - (emulation ? 64'h2 : (width_64 ? 64'h8 : 64'h2));
                        cycle <= 4'b0001;
                        width <= emulation ? 3'b001 : (width_64 ? 3'b111 : 3'b001);
                    end
                    4'b0001: begin
                        // Push Status Register
                        addr <= S;
                        data_out <= {48'h0, P};
                        rw <= 1'b0;
                        S <= S - (emulation ? 64'h1 : (width_64 ? 64'h8 : 64'h2));
                        cycle <= 4'b0010;
                        width <= emulation ? 3'b000 : (width_64 ? 3'b111 : 3'b001);
                    end
                    4'b0010: begin
                        // Set interrupt flags
                        P[10] <= 1'b1;  // Set I flag
                        P[11] <= 1'b0;  // Clear D flag
                        
                        // Determine vector address
                        if (nmi_latch) begin
                            addr <= {56'h0, 16'hFFFA};
                            nmi_latch <= 1'b0;
                        end else if (irq_latch) begin
                            addr <= {56'h0, 16'hFFFE};
                            irq_latch <= 1'b0;
                        end else if (abrt_latch) begin
                            addr <= {56'h0, 16'hFFF8};
                            abrt_latch <= 1'b0;
                        end else if (opcode == BRK) begin
                            addr <= {56'h0, 16'hFFFE};
                        end else if (opcode == COP) begin
                            addr <= {56'h0, 16'hFFF4};
                        end
                        cycle <= 4'b0011;
                        width <= 3'b001;
                    end
                    4'b0011: begin
                        // Read vector low
                        TEMP[15:0] <= data_in[15:0];
                        addr <= addr + (emulation ? 64'h1 : 64'h2);
                        cycle <= 4'b0100;
                        width <= 3'b001;
                    end
                    4'b0100: begin
                        // Read vector high and jump
                        if (emulation) begin
                            PC <= {56'h0, data_in[7:0], TEMP[7:0]};
                            K <= 64'h0;
                        } else if (width_64) begin
                            PC <= {K[55:0], data_in[15:0], TEMP[15:0]};
                        end else begin
                            PC <= {K[55:0], 48'h0, data_in[15:0], TEMP[15:0]};
                        end
                        state <= FETCH;
                    end
                endcase
            end
            WAIT: begin
                if (nmi || irq || abrt) begin
                    state <= INTERRUPT;
                end
            end
            HALT: begin
                // Processor halted until reset
            end
        endcase
    end
end

// Mode detection logic
always_comb begin
    if (emulation) begin
        cpu_mode = MODE_6502;
        width_64 = 1'b0;
        size_mode = 2'b00;
    end else if (native && ~width_64) begin
        cpu_mode = MODE_65816;
        width_64 = 1'b0;
        size_mode = {m_flag, x_flag};
    end else if (native && width_64) begin
        cpu_mode = MODE_65864;
        size_mode = 2'b11;
    end else begin
        cpu_mode = MODE_65C02;
        width_64 = 1'b0;
        size_mode = 2'b00;
    end
end

assign e = emulation;

endmodule
