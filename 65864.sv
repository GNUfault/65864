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
    ADC_IMM = 8'h69, ADC_DIR = 8'h65, ADC_DIRX = 8'h75, ADC_ABS = 8'h6D,
    ADC_ABSX = 8'h7D, ADC_ABSY = 8'h79, ADC_INDX = 8'h61, ADC_INDY = 8'h71,
    ADC_IND = 8'h72, ADC_INDY_L = 8'h77, ADC_STK = 8'h63, ADC_STKY = 8'h73,
    ADC_LONG = 8'h67, ADC_LONGX = 8'h7F, AND_IMM = 8'h29, AND_DIR = 8'h25,
    AND_DIRX = 8'h35, AND_ABS = 8'h2D, AND_ABSX = 8'h3D, AND_ABSY = 8'h39,
    AND_INDX = 8'h21, AND_INDY = 8'h31, AND_IND = 8'h32, AND_INDY_L = 8'h37,
    AND_STK = 8'h23, AND_STKY = 8'h33, AND_LONG = 8'h27, AND_LONGX = 8'h3F,
    ASL_ACC = 8'h0A, ASL_DIR = 8'h06, ASL_DIRX = 8'h16, ASL_ABS = 8'h0E,
    ASL_ABSX = 8'h1E, ASL_LONG = 8'h0F, ASL_LONGX = 8'h1F, BCC_REL = 8'h90,
    BCS_REL = 8'hB0, BEQ_REL = 8'hF0, BIT_IMM = 8'h89, BIT_DIR = 8'h24,
    BIT_DIRX = 8'h34, BIT_ABS = 8'h2C, BIT_ABSX = 8'h3C, BIT_LONG = 8'h2F,
    BIT_LONGX = 8'h3F, BMI_REL = 8'h30, BNE_REL = 8'hD0, BPL_REL = 8'h10,
    BRA_REL = 8'h80, BRK_S = 8'h00, BRL_REL = 8'h82, BVC_REL = 8'h50,
    BVS_REL = 8'h70, CLC_C = 8'h18, CLD_D = 8'hD8, CLI_I = 8'h58,
    CLV_V = 8'hB8, CMP_IMM = 8'hC9, CMP_DIR = 8'hC5, CMP_DIRX = 8'hD5,
    CMP_ABS = 8'hCD, CMP_ABSX = 8'hDD, CMP_ABSY = 8'hD9, CMP_INDX = 8'hC1,
    CMP_INDY = 8'hD1, CMP_IND = 8'hD2, CMP_INDY_L = 8'hD7, CMP_STK = 8'hC3,
    CMP_STKY = 8'hD3, CMP_LONG = 8'hC7, CMP_LONGX = 8'hDF, COP_S = 8'h02,
    CPX_IMM = 8'hE0, CPX_DIR = 8'hE4, CPX_ABS = 8'hEC, CPY_IMM = 8'hC0,
    CPY_DIR = 8'hC4, CPY_ABS = 8'hCC, DEC_ACC = 8'h3A, DEC_DIR = 8'hC6,
    DEC_DIRX = 8'hD6, DEC_ABS = 8'hCE, DEC_ABSX = 8'hDE, DEC_LONG = 8'hCF,
    DEC_LONGX = 8'hDF, DEX_X = 8'hCA, DEY_Y = 8'h88, EOR_IMM = 8'h49,
    EOR_DIR = 8'h45, EOR_DIRX = 8'h55, EOR_ABS = 8'h4D, EOR_ABSX = 8'h5D,
    EOR_ABSY = 8'h59, EOR_INDX = 8'h41, EOR_INDY = 8'h51, EOR_IND = 8'h52,
    EOR_INDY_L = 8'h57, EOR_STK = 8'h43, EOR_STKY = 8'h53, EOR_LONG = 8'h47,
    EOR_LONGX = 8'h5F, INC_ACC = 8'h1A, INC_DIR = 8'hE6, INC_DIRX = 8'hF6,
    INC_ABS = 8'hEE, INC_ABSX = 8'hFE, INC_LONG = 8'hEF, INC_LONGX = 8'hFF,
    INX_X = 8'hE8, INY_Y = 8'hC8, JMP_ABS = 8'h4C, JMP_IND = 8'h6C,
    JMP_INDX = 8'h7C, JMP_LONG = 8'h5C, JSR_ABS = 8'h20, JSR_INDX = 8'hFC,
    JSR_LONG = 8'h22, LDA_IMM = 8'hA9, LDA_DIR = 8'hA5, LDA_DIRX = 8'hB5,
    LDA_ABS = 8'hAD, LDA_ABSX = 8'hBD, LDA_ABSY = 8'hB9, LDA_INDX = 8'hA1,
    LDA_INDY = 8'hB1, LDA_IND = 8'hB2, LDA_INDY_L = 8'hB7, LDA_STK = 8'hA3,
    LDA_STKY = 8'hB3, LDA_LONG = 8'hA7, LDA_LONGX = 8'hBF, LDX_IMM = 8'hA2,
    LDX_DIR = 8'hA6, LDX_DIRY = 8'hB6, LDX_ABS = 8'hAE, LDX_ABSY = 8'hBE,
    LDX_LONG = 8'hAF, LDX_LONGY = 8'hBF, LDY_IMM = 8'hA0, LDY_DIR = 8'hA4,
    LDY_DIRX = 8'hB4, LDY_ABS = 8'hAC, LDY_ABSX = 8'hBC, LDY_LONG = 8'hAC,
    LDY_LONGX = 8'hBC, LSR_ACC = 8'h4A, LSR_DIR = 8'h46, LSR_DIRX = 8'h56,
    LSR_ABS = 8'h4E, LSR_ABSX = 8'h5E, LSR_LONG = 8'h4F, LSR_LONGX = 8'h5F,
    MVN_BM = 8'h54, MVP_BM = 8'h44, NOP_S = 8'hEA, ORA_IMM = 8'h09,
    ORA_DIR = 8'h05, ORA_DIRX = 8'h15, ORA_ABS = 8'h0D, ORA_ABSX = 8'h1D,
    ORA_ABSY = 8'h19, ORA_INDX = 8'h01, ORA_INDY = 8'h11, ORA_IND = 8'h12,
    ORA_INDY_L = 8'h17, ORA_STK = 8'h03, ORA_STKY = 8'h13, ORA_LONG = 8'h07,
    ORA_LONGX = 8'h1F, PEA_S = 8'hF4, PEI_S = 8'hD4, PER_S = 8'h62,
    PHA_S = 8'h48, PHB_S = 8'h8B, PHD_S = 8'h0B, PHK_S = 8'h4B,
    PHP_S = 8'h08, PHX_S = 8'hDA, PHY_S = 8'h5A, PLA_S = 8'h68,
    PLB_S = 8'hAB, PLD_S = 8'h2B, PLP_S = 8'h28, PLX_S = 8'hFA,
    PLY_S = 8'h7A, REP_S = 8'hC2, ROL_ACC = 8'h2A, ROL_DIR = 8'h26,
    ROL_DIRX = 8'h36, ROL_ABS = 8'h2E, ROL_ABSX = 8'h3E, ROL_LONG = 8'h2F,
    ROL_LONGX = 8'h3F, ROR_ACC = 8'h6A, ROR_DIR = 8'h66, ROR_DIRX = 8'h76,
    ROR_ABS = 8'h6E, ROR_ABSX = 8'h7E, ROR_LONG = 8'h6F, ROR_LONGX = 8'h7F,
    RTI_S = 8'h40, RTL_S = 8'h6B, RTS_S = 8'h60, SBC_IMM = 8'hE9,
    SBC_DIR = 8'hE5, SBC_DIRX = 8'hF5, SBC_ABS = 8'hED, SBC_ABSX = 8'hFD,
    SBC_ABSY = 8'hF9, SBC_INDX = 8'hE1, SBC_INDY = 8'hF1, SBC_IND = 8'hF2,
    SBC_INDY_L = 8'hF7, SBC_STK = 8'hE3, SBC_STKY = 8'hF3, SBC_LONG = 8'hE7,
    SBC_LONGX = 8'hFF, SEC_C = 8'h38, SED_D = 8'hF8, SEI_I = 8'h78,
    SEP_S = 8'hE2, STA_DIR = 8'h85, STA_DIRX = 8'h95, STA_ABS = 8'h8D,
    STA_ABSX = 8'h9D, STA_ABSY = 8'h99, STA_INDX = 8'h81, STA_INDY = 8'h91,
    STA_IND = 8'h92, STA_INDY_L = 8'h97, STA_STK = 8'h83, STA_STKY = 8'h93,
    STA_LONG = 8'h87, STA_LONGX = 8'h9F, STP_S = 8'hDB, STX_DIR = 8'h86,
    STX_DIRY = 8'h96, STX_ABS = 8'h8E, STX_LONG = 8'h8F, STY_DIR = 8'h84,
    STY_DIRX = 8'h94, STY_ABS = 8'h8C, STY_LONG = 8'h8C, STZ_DIR = 8'h64,
    STZ_DIRX = 8'h74, STZ_ABS = 8'h9C, STZ_ABSX = 8'h9E, TAX_S = 8'hAA,
    TAY_S = 8'hA8, TCD_S = 8'h5B, TCS_S = 8'h1B, TDC_S = 8'h7B,
    TRB_DIR = 8'h14, TRB_ABS = 8'h1C, TSB_DIR = 8'h04, TSB_ABS = 8'h0C,
    TSC_S = 8'h3B, TSX_S = 8'hBA, TXA_S = 8'h8A, TXS_S = 8'h9A,
    TXY_S = 8'h9B, TYA_S = 8'h98, TYX_S = 8'hBB, WAI_S = 8'hCB,
    WDM_S = 8'h42, XBA_S = 8'hEB, XCE_S = 8'hFB
} opcode_t;

typedef enum logic [3:0] {
    FETCH,
    DECODE,
    EXEC1,
    EXEC2,
    EXEC3,
    EXEC4,
    EXEC5,
    STORE1,
    STORE2,
    STORE3,
    STACK1,
    STACK2,
    STACK3,
    INTERRUPT,
    WAIT
} state_t;

logic [63:0] A;
logic [63:0] X;
logic [63:0] Y;
logic [63:0] S;
logic [63:0] D;
logic [63:0] B;
logic [63:0] PC;
logic [63:0] IR;
logic [63:0] ADR;
logic [63:0] TEMP;
logic [63:0] DBR;
logic [7:0] PBR;
logic [7:0] K;
logic [15:0] P;
logic [7:0] opcode;
state_t state;
logic [3:0] cycle;
logic nmi_latch;
logic irq_latch;
logic abrt_latch;
logic native;
logic emulation;
logic width_64;
logic [2:0] size_mode;
logic m_flag;
logic x_flag;

wire N = P[15];
wire V = P[14];
wire M = P[13];
wire XF = P[12];
wire D = P[11];
wire I = P[10];
wire Z = P[9];
wire C = P[8];
wire E = emulation;

always_ff @(posedge clk) begin
    if (reset) begin
        A <= 64'h0000000000000000;
        X <= 64'h0000000000000000;
        Y <= 64'h0000000000000000;
        S <= 64'h000000000001FF;
        D <= 64'h0000000000000000;
        B <= 64'h0000000000000000;
        PC <= {56'h00000000000000, 16'hFFFC};
        P <= 16'h3000;
        DBR <= 64'h0000000000000000;
        PBR <= 8'h00;
        K <= 8'h00;
        state <= FETCH;
        cycle <= 4'b0000;
        rw <= 1'b1;
        sync <= 1'b0;
        nmi_latch <= 1'b0;
        irq_latch <= 1'b0;
        abrt_latch <= 1'b0;
        native <= 1'b1;
        emulation <= 1'b0;
        width_64 <= 1'b1;
        m_flag <= 1'b0;
        x_flag <= 1'b0;
        width <= 3'b111;
    end else if (ready) begin
        case (state)
            FETCH: begin
                sync <= 1'b1;
                addr <= {DBR[55:0], PC[63:0]};
                rw <= 1'b1;
                PC <= PC + 64'h1;
                state <= DECODE;
                width <= 3'b111;
            end
            DECODE: begin
                sync <= 1'b0;
                opcode <= data_in[7:0];
                IR <= data_in;
                cycle <= 4'b0000;
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
                case (opcode)
                    ADC_IMM: begin
                        addr <= {DBR[55:0], PC[63:0]};
                        PC <= PC + (width_64 ? 64'h8 : (m_flag ? 64'h2 : 64'h1));
                        TEMP <= data_in;
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    ADC_DIR: begin
                        addr <= PC;
                        PC <= PC + 64'h1;
                        state <= EXEC2;
                        width <= 3'b000;
                    end
                    ADC_ABS: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h2);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b001;
                    end
                    ADC_LONG: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h3);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b010;
                    end
                    LDA_IMM: begin
                        addr <= {DBR[55:0], PC[63:0]};
                        PC <= PC + (width_64 ? 64'h8 : (m_flag ? 64'h2 : 64'h1));
                        TEMP <= data_in;
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    LDA_DIR: begin
                        addr <= PC;
                        PC <= PC + 64'h1;
                        state <= EXEC2;
                        width <= 3'b000;
                    end
                    LDA_ABS: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h2);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b001;
                    end
                    LDA_LONG: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h3);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b010;
                    end
                    STA_DIR: begin
                        addr <= PC;
                        PC <= PC + 64'h1;
                        state <= EXEC2;
                        width <= 3'b000;
                    end
                    STA_ABS: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h2);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b001;
                    end
                    STA_LONG: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h3);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b010;
                    end
                    JMP_ABS: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h2);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b001;
                    end
                    JMP_LONG: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h3);
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : 3'b010;
                    end
                    JSR_ABS: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h2);
                        state <= STACK1;
                        width <= width_64 ? 3'b111 : 3'b001;
                    end
                    JSR_LONG: begin
                        addr <= PC;
                        PC <= PC + (width_64 ? 64'h8 : 64'h3);
                        state <= STACK1;
                        width <= width_64 ? 3'b111 : 3'b010;
                    end
                    RTS_S: begin
                        S <= S + 64'h8;
                        addr <= S;
                        state <= EXEC2;
                        width <= 3'b111;
                    end
                    RTL_S: begin
                        S <= S + 64'h8;
                        addr <= S;
                        state <= EXEC2;
                        width <= 3'b111;
                    end
                    PHA_S: begin
                        addr <= S;
                        data_out <= A;
                        rw <= 1'b0;
                        S <= S - (width_64 ? 64'h8 : (m_flag ? 64'h2 : 64'h1));
                        state <= FETCH;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    PLA_S: begin
                        S <= S + (width_64 ? 64'h8 : (m_flag ? 64'h2 : 64'h1));
                        addr <= S;
                        state <= EXEC2;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    TAX_S: begin
                        X <= A;
                        P[9] <= (A == 64'h0);
                        P[15] <= A[63];
                        state <= FETCH;
                    end
                    TAY_S: begin
                        Y <= A;
                        P[9] <= (A == 64'h0);
                        P[15] <= A[63];
                        state <= FETCH;
                    end
                    TXA_S: begin
                        A <= X;
                        P[9] <= (X == 64'h0);
                        P[15] <= X[63];
                        state <= FETCH;
                    end
                    TYA_S: begin
                        A <= Y;
                        P[9] <= (Y == 64'h0);
                        P[15] <= Y[63];
                        state <= FETCH;
                    end
                    INX_X: begin
                        X <= X + 64'h1;
                        P[9] <= ((X + 64'h1) == 64'h0);
                        P[15] <= (X + 64'h1)[63];
                        state <= FETCH;
                    end
                    INY_Y: begin
                        Y <= Y + 64'h1;
                        P[9] <= ((Y + 64'h1) == 64'h0);
                        P[15] <= (Y + 64'h1)[63];
                        state <= FETCH;
                    end
                    DEX_X: begin
                        X <= X - 64'h1;
                        P[9] <= ((X - 64'h1) == 64'h0);
                        P[15] <= (X - 64'h1)[63];
                        state <= FETCH;
                    end
                    DEY_Y: begin
                        Y <= Y - 64'h1;
                        P[9] <= ((Y - 64'h1) == 64'h0);
                        P[15] <= (Y - 64'h1)[63];
                        state <= FETCH;
                    end
                    SEC_C: begin
                        P[8] <= 1'b1;
                        state <= FETCH;
                    end
                    CLC_C: begin
                        P[8] <= 1'b0;
                        state <= FETCH;
                    end
                    SEI_I: begin
                        P[10] <= 1'b1;
                        state <= FETCH;
                    end
                    CLI_I: begin
                        P[10] <= 1'b0;
                        state <= FETCH;
                    end
                    SED_D: begin
                        P[11] <= 1'b1;
                        state <= FETCH;
                    end
                    CLD_D: begin
                        P[11] <= 1'b0;
                        state <= FETCH;
                    end
                    CLV_V: begin
                        P[14] <= 1'b0;
                        state <= FETCH;
                    end
                    REP_S: begin
                        addr <= PC;
                        PC <= PC + 64'h1;
                        state <= EXEC2;
                        width <= 3'b000;
                    end
                    SEP_S: begin
                        addr <= PC;
                        PC <= PC + 64'h1;
                        state <= EXEC2;
                        width <= 3'b000;
                    end
                    XCE_S: begin
                        temp = {emulation, C};
                        emulation <= temp[0];
                        P[8] <= temp[1];
                        state <= FETCH;
                    end
                    NOP_S: begin
                        state <= FETCH;
                    end
                    WAI_S: begin
                        state <= WAIT;
                    end
                    STP_S: begin
                        state <= WAIT;
                    end
                    COP_S: begin
                        state <= INTERRUPT;
                        cycle <= 4'b0000;
                    end
                    BRK_S: begin
                        state <= INTERRUPT;
                        cycle <= 4'b0000;
                    end
                    RTI_S: begin
                        S <= S + 64'h8;
                        addr <= S;
                        state <= EXEC2;
                        width <= 3'b111;
                    end
                    MVN_BM: begin
                        addr <= PC;
                        PC <= PC + 64'h2;
                        state <= EXEC2;
                        width <= 3'b000;
                    end
                    MVP_BM: begin
                        addr <= PC;
                        PC <= PC + 64'h2;
                        state <= EXEC2;
                        width <= 3'b000;
                    end
                    default: begin
                        state <= FETCH;
                    end
                endcase
            end
            EXEC2: begin
                case (opcode)
                    ADC_IMM: begin
                        if (width_64) begin
                            A <= A + TEMP + (C ? 64'h1 : 64'h0);
                            P[8] <= (A + TEMP + (C ? 64'h1 : 64'h0)) < A;
                            P[9] <= ((A + TEMP + (C ? 64'h1 : 64'h0)) == 64'h0);
                            P[14] <= (~A[63] & ~TEMP[63] & (A + TEMP + (C ? 64'h1 : 64'h0))[63]) | 
                                    (A[63] & TEMP[63] & ~(A + TEMP + (C ? 64'h1 : 64'h0))[63]);
                            P[15] <= (A + TEMP + (C ? 64'h1 : 64'h0))[63];
                        end else if (m_flag) begin
                            A[15:0] <= A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0);
                            P[8] <= (A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0)) < A[15:0];
                            P[9] <= ((A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0)) == 16'h0);
                            P[14] <= (~A[15] & ~TEMP[15] & (A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0))[15]) | 
                                    (A[15] & TEMP[15] & ~(A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0))[15]);
                            P[15] <= (A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0))[15];
                        end else begin
                            A[7:0] <= A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0);
                            P[8] <= (A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0)) < A[7:0];
                            P[9] <= ((A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0)) == 8'h0);
                            P[14] <= (~A[7] & ~TEMP[7] & (A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0))[7]) | 
                                    (A[7] & TEMP[7] & ~(A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0))[7]);
                            P[15] <= (A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0))[7];
                        end
                        state <= FETCH;
                    end
                    ADC_DIR: begin
                        ADR <= {56'h0, data_in[7:0]} + D;
                        state <= EXEC3;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    ADC_ABS: begin
                        if (width_64) begin
                            ADR <= {DBR[55:0], data_in[63:0]};
                        end else begin
                            ADR <= {DBR[55:0], 48'h0, data_in[15:0]};
                        end
                        state <= EXEC3;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    ADC_LONG: begin
                        if (width_64) begin
                            ADR <= data_in[63:0];
                        end else begin
                            ADR <= {40'h0, data_in[23:0]};
                        end
                        state <= EXEC3;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
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
                    LDA_DIR: begin
                        ADR <= {56'h0, data_in[7:0]} + D;
                        state <= EXEC3;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    LDA_ABS: begin
                        if (width_64) begin
                            ADR <= {DBR[55:0], data_in[63:0]};
                        end else begin
                            ADR <= {DBR[55:0], 48'h0, data_in[15:0]};
                        end
                        state <= EXEC3;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    LDA_LONG: begin
                        if (width_64) begin
                            ADR <= data_in[63:0];
                        end else begin
                            ADR <= {40'h0, data_in[23:0]};
                        end
                        state <= EXEC3;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    STA_DIR: begin
                        ADR <= {56'h0, data_in[7:0]} + D;
                        if (width_64) begin
                            data_out <= A;
                        end else if (m_flag) begin
                            data_out <= {48'h0, A[15:0]};
                        end else begin
                            data_out <= {56'h0, A[7:0]};
                        end
                        rw <= 1'b0;
                        state <= FETCH;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    STA_ABS: begin
                        if (width_64) begin
                            ADR <= {DBR[55:0], data_in[63:0]};
                            data_out <= A;
                        end else begin
                            ADR <= {DBR[55:0], 48'h0, data_in[15:0]};
                            data_out <= {48'h0, A[15:0]};
                        end
                        rw <= 1'b0;
                        state <= FETCH;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    STA_LONG: begin
                        if (width_64) begin
                            ADR <= data_in[63:0];
                            data_out <= A;
                        end else begin
                            ADR <= {40'h0, data_in[23:0]};
                            data_out <= {48'h0, A[15:0]};
                        end
                        rw <= 1'b0;
                        state <= FETCH;
                        width <= width_64 ? 3'b111 : (m_flag ? 3'b001 : 3'b000);
                    end
                    JMP_ABS: begin
                        if (width_64) begin
                            PC <= {DBR[55:0], data_in[63:0]};
                        end else begin
                            PC <= {DBR[55:0], 48'h0, data_in[15:0]};
                        end
                        state <= FETCH;
                    end
                    JMP_LONG: begin
                        if (width_64) begin
                            PC <= data_in[63:0];
                        end else begin
                            PC <= {40'h0, data_in[23:0]};
                        end
                        state <= FETCH;
                    end
                    RTS_S: begin
                        PC <= {DBR[55:0], data_in[63:0]};
                        PC <= PC + 64'h1;
                        state <= FETCH;
                    end
                    RTL_S: begin
                        PC <= data_in[63:0];
                        PC <= PC + 64'h1;
                        state <= FETCH;
                    end
                    PLA_S: begin
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
                        state <= FETCH;
                    end
                    REP_S: begin
                        P <= P & ~data_in[15:0];
                        m_flag <= ~data_in[13];
                        x_flag <= ~data_in[12];
                        state <= FETCH;
                    end
                    SEP_S: begin
                        P <= P | data_in[15:0];
                        m_flag <= data_in[13];
                        x_flag <= data_in[12];
                        state <= FETCH;
                    end
                    RTI_S: begin
                        P <= data_in[15:0];
                        S <= S + 64'h8;
                        addr <= S;
                        state <= EXEC3;
                        width <= 3'b111;
                    end
                    MVN_BM: begin
                        B <= {56'h0, data_in[15:8]};
                        DBR <= {56'h0, data_in[7:0]};
                        state <= EXEC3;
                    end
                    MVP_BM: begin
                        B <= {56'h0, data_in[15:8]};
                        DBR <= {56'h0, data_in[7:0]};
                        state <= EXEC3;
                    end
                    default: begin
                        state <= FETCH;
                    end
                endcase
            end
            EXEC3: begin
                case (opcode)
                    ADC_DIR, ADC_ABS, ADC_LONG: begin
                        addr <= ADR;
                        state <= EXEC4;
                    end
                    LDA_DIR, LDA_ABS, LDA_LONG: begin
                        addr <= ADR;
                        state <= EXEC4;
                    end
                    RTI_S: begin
                        PC <= {DBR[55:0], data_in[63:0]};
                        state <= FETCH;
                    end
                    MVN_BM: begin
                        addr <= {DBR[55:0], X[63:0]};
                        state <= EXEC4;
                    end
                    MVP_BM: begin
                        addr <= {DBR[55:0], X[63:0]};
                        state <= EXEC4;
                    end
                    default: begin
                        state <= FETCH;
                    end
                endcase
            end
            EXEC4: begin
                case (opcode)
                    ADC_DIR, ADC_ABS, ADC_LONG: begin
                        TEMP <= data_in;
                        state <= EXEC5;
                    end
                    LDA_DIR, LDA_ABS, LDA_LONG: begin
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
                        state <= FETCH;
                    end
                    MVN_BM: begin
                        TEMP <= data_in;
                        addr <= {B[55:0], Y[63:0]};
                        state <= EXEC5;
                    end
                    MVP_BM: begin
                        TEMP <= data_in;
                        addr <= {B[55:0], Y[63:0]};
                        state <= EXEC5;
                    end
                    default: begin
                        state <= FETCH;
                    end
                endcase
            end
            EXEC5: begin
                case (opcode)
                    ADC_DIR, ADC_ABS, ADC_LONG: begin
                        if (width_64) begin
                            A <= A + TEMP + (C ? 64'h1 : 64'h0);
                            P[8] <= (A + TEMP + (C ? 64'h1 : 64'h0)) < A;
                            P[9] <= ((A + TEMP + (C ? 64'h1 : 64'h0)) == 64'h0);
                            P[14] <= (~A[63] & ~TEMP[63] & (A + TEMP + (C ? 64'h1 : 64'h0))[63]) | 
                                    (A[63] & TEMP[63] & ~(A + TEMP + (C ? 64'h1 : 64'h0))[63]);
                            P[15] <= (A + TEMP + (C ? 64'h1 : 64'h0))[63];
                        end else if (m_flag) begin
                            A[15:0] <= A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0);
                            P[8] <= (A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0)) < A[15:0];
                            P[9] <= ((A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0)) == 16'h0);
                            P[14] <= (~A[15] & ~TEMP[15] & (A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0))[15]) | 
                                    (A[15] & TEMP[15] & ~(A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0))[15]);
                            P[15] <= (A[15:0] + TEMP[15:0] + (C ? 16'h1 : 16'h0))[15];
                        end else begin
                            A[7:0] <= A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0);
                            P[8] <= (A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0)) < A[7:0];
                            P[9] <= ((A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0)) == 8'h0);
                            P[14] <= (~A[7] & ~TEMP[7] & (A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0))[7]) | 
                                    (A[7] & TEMP[7] & ~(A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0))[7]);
                            P[15] <= (A[7:0] + TEMP[7:0] + (C ? 8'h1 : 8'h0))[7];
                        end
                        state <= FETCH;
                    end
                    MVN_BM: begin
                        data_out <= TEMP;
                        rw <= 1'b0;
                        addr <= {B[55:0], Y[63:0]};
                        X <= X + 64'h1;
                        Y <= Y + 64'h1;
                        A <= A - 64'h1;
                        if (A != 64'h0) begin
                            addr <= {DBR[55:0], X[63:0]};
                            state <= EXEC4;
                        end else begin
                            state <= FETCH;
                        end
                    end
                    MVP_BM: begin
                        data_out <= TEMP;
                        rw <= 1'b0;
                        addr <= {B[55:0], Y[63:0]};
                        X <= X - 64'h1;
                        Y <= Y - 64'h1;
                        A <= A - 64'h1;
                        if (A != 64'h0) begin
                            addr <= {DBR[55:0], X[63:0]};
                            state <= EXEC4;
                        end else begin
                            state <= FETCH;
                        end
                    end
                    default: begin
                        state <= FETCH;
                    end
                endcase
            end
            STACK1: begin
                addr <= S;
                if (opcode == JSR_ABS) begin
                    data_out <= PC[63:0];
                end else begin
                    data_out <= {DBR[55:0], PC[63:0]};
                end
                rw <= 1'b0;
                S <= S - (width_64 ? 64'h8 : 64'h2);
                state <= STACK2;
                width <= width_64 ? 3'b111 : 3'b001;
            end
            STACK2: begin
                if (opcode == JSR_ABS) begin
                    if (width_64) begin
                        PC <= {DBR[55:0], TEMP[63:0]};
                    end else begin
                        PC <= {DBR[55:0], 48'h0, TEMP[15:0]};
                    end
                end else begin
                    if (width_64) begin
                        PC <= TEMP[63:0];
                    end else begin
                        PC <= {40'h0, TEMP[23:0]};
                    end
                end
                state <= FETCH;
            end
            INTERRUPT: begin
                case (cycle)
                    4'b0000: begin
                        addr <= S;
                        data_out <= {DBR[55:0], PC[63:0]};
                        rw <= 1'b0;
                        S <= S - 64'h8;
                        cycle <= 4'b0001;
                        width <= 3'b111;
                    end
                    4'b0001: begin
                        addr <= S;
                        data_out <= {48'h0, P};
                        rw <= 1'b0;
                        S <= S - 64'h8;
                        cycle <= 4'b0010;
                        width <= 3'b001;
                    end
                    4'b0010: begin
                        P[10] <= 1'b1;
                        P[11] <= 1'b0;
                        if (nmi_latch) begin
                            addr <= {56'h0, 16'hFFFA};
                            nmi_latch <= 1'b0;
                        end else if (irq_latch) begin
                            addr <= {56'h0, 16'hFFFE};
                            irq_latch <= 1'b0;
                        end else if (abrt_latch) begin
                            addr <= {56'h0, 16'hFFF8};
                            abrt_latch <= 1'b0;
                        end else if (opcode == BRK_S) begin
                            addr <= {56'h0, 16'hFFFE};
                        end else if (opcode == COP_S) begin
                            addr <= {56'h0, 16'hFFF4};
                        end
                        cycle <= 4'b0011;
                        width <= 3'b001;
                    end
                    4'b0011: begin
                        TEMP[15:0] <= data_in[15:0];
                        addr <= addr + 64'h2;
                        cycle <= 4'b0100;
                        width <= 3'b001;
                    end
                    4'b0100: begin
                        PC <= {DBR[55:0], data_in[15:0], TEMP[15:0]};
                        state <= FETCH;
                    end
                endcase
            end
            WAIT: begin
                if (nmi || irq || abrt) begin
                    state <= INTERRUPT;
                end
            end
            default: begin
                state <= FETCH;
            end
        endcase
    end
end

always_comb begin
    if (emulation) begin
        width_64 = 1'b0;
        size_mode = 2'b00;
    end else begin
        width_64 = ~m_flag;
        size_mode = {m_flag, x_flag};
    end
end

endmodule
