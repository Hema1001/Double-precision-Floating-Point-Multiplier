//
// Generated by Bluespec Compiler, version 2021.12.1 (build fd501401)
//
// On Wed Dec 13 11:09:56 IST 2023
//
//
// Ports:
// Name                         I/O  size props
// RDY_send                       O     1
// receive                        O    64 reg
// RDY_receive                    O     1
// CLK                            I     1 clock
// RST_N                          I     1 reset
// send_data_in                   I   128 reg
// EN_send                        I     1
// EN_receive                     I     1
//
// Combinational paths from inputs to outputs:
//   EN_receive -> RDY_send
//
//

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module mkPDpMul(CLK,
		RST_N,

		send_data_in,
		EN_send,
		RDY_send,

		EN_receive,
		receive,
		RDY_receive);
  input  CLK;
  input  RST_N;

  // action method send
  input  [127 : 0] send_data_in;
  input  EN_send;
  output RDY_send;

  // actionvalue method receive
  input  EN_receive;
  output [63 : 0] receive;
  output RDY_receive;

  // signals for module outputs
  wire [63 : 0] receive;
  wire RDY_receive, RDY_send;

  // register d_copy1
  reg [127 : 0] d_copy1;
  wire [127 : 0] d_copy1$D_IN;
  wire d_copy1$EN;

  // register r_sgn_1
  reg r_sgn_1;
  wire r_sgn_1$D_IN, r_sgn_1$EN;

  // register r_sgn_2
  reg r_sgn_2;
  wire r_sgn_2$D_IN, r_sgn_2$EN;

  // register r_sgn_3
  reg r_sgn_3;
  wire r_sgn_3$D_IN, r_sgn_3$EN;

  // register r_sgn_4
  reg r_sgn_4;
  wire r_sgn_4$D_IN, r_sgn_4$EN;

  // register r_sgn_fin
  reg r_sgn_fin;
  wire r_sgn_fin$D_IN, r_sgn_fin$EN;

  // register rg_exp1_1
  reg [10 : 0] rg_exp1_1;
  wire [10 : 0] rg_exp1_1$D_IN;
  wire rg_exp1_1$EN;

  // register rg_exp2_1
  reg [10 : 0] rg_exp2_1;
  wire [10 : 0] rg_exp2_1$D_IN;
  wire rg_exp2_1$EN;

  // register rg_fin_exp
  reg [11 : 0] rg_fin_exp;
  wire [11 : 0] rg_fin_exp$D_IN;
  wire rg_fin_exp$EN;

  // register rg_fin_m
  reg [51 : 0] rg_fin_m;
  wire [51 : 0] rg_fin_m$D_IN;
  wire rg_fin_m$EN;

  // register rg_prod_par012
  reg [105 : 0] rg_prod_par012;
  wire [105 : 0] rg_prod_par012$D_IN;
  wire rg_prod_par012$EN;

  // register rg_prod_par0_1
  reg [105 : 0] rg_prod_par0_1;
  wire [105 : 0] rg_prod_par0_1$D_IN;
  wire rg_prod_par0_1$EN;

  // register rg_prod_par0_2
  reg [105 : 0] rg_prod_par0_2;
  wire [105 : 0] rg_prod_par0_2$D_IN;
  wire rg_prod_par0_2$EN;

  // register rg_prod_par1_1
  reg [105 : 0] rg_prod_par1_1;
  wire [105 : 0] rg_prod_par1_1$D_IN;
  wire rg_prod_par1_1$EN;

  // register rg_prod_par1_2
  reg [105 : 0] rg_prod_par1_2;
  wire [105 : 0] rg_prod_par1_2$D_IN;
  wire rg_prod_par1_2$EN;

  // register rg_prod_par2_1
  reg [105 : 0] rg_prod_par2_1;
  wire [105 : 0] rg_prod_par2_1$D_IN;
  wire rg_prod_par2_1$EN;

  // register rg_prod_par2_2
  reg [105 : 0] rg_prod_par2_2;
  wire [105 : 0] rg_prod_par2_2$D_IN;
  wire rg_prod_par2_2$EN;

  // register rg_prod_par345
  reg [105 : 0] rg_prod_par345;
  wire [105 : 0] rg_prod_par345$D_IN;
  wire rg_prod_par345$EN;

  // register rg_prod_par3_1
  reg [105 : 0] rg_prod_par3_1;
  wire [105 : 0] rg_prod_par3_1$D_IN;
  wire rg_prod_par3_1$EN;

  // register rg_prod_par3_2
  reg [105 : 0] rg_prod_par3_2;
  wire [105 : 0] rg_prod_par3_2$D_IN;
  wire rg_prod_par3_2$EN;

  // register rg_prod_par4_1
  reg [105 : 0] rg_prod_par4_1;
  wire [105 : 0] rg_prod_par4_1$D_IN;
  wire rg_prod_par4_1$EN;

  // register rg_prod_par4_2
  reg [105 : 0] rg_prod_par4_2;
  wire [105 : 0] rg_prod_par4_2$D_IN;
  wire rg_prod_par4_2$EN;

  // register rg_prod_par5_1
  reg [105 : 0] rg_prod_par5_1;
  wire [105 : 0] rg_prod_par5_1$D_IN;
  wire rg_prod_par5_1$EN;

  // register rg_prod_par5_2
  reg [105 : 0] rg_prod_par5_2;
  wire [105 : 0] rg_prod_par5_2$D_IN;
  wire rg_prod_par5_2$EN;

  // register rg_prod_par678
  reg [105 : 0] rg_prod_par678;
  wire [105 : 0] rg_prod_par678$D_IN;
  wire rg_prod_par678$EN;

  // register rg_prod_par6_1
  reg [105 : 0] rg_prod_par6_1;
  wire [105 : 0] rg_prod_par6_1$D_IN;
  wire rg_prod_par6_1$EN;

  // register rg_prod_par6_2
  reg [105 : 0] rg_prod_par6_2;
  wire [105 : 0] rg_prod_par6_2$D_IN;
  wire rg_prod_par6_2$EN;

  // register rg_prod_par7_1
  reg [105 : 0] rg_prod_par7_1;
  wire [105 : 0] rg_prod_par7_1$D_IN;
  wire rg_prod_par7_1$EN;

  // register rg_prod_par7_2
  reg [105 : 0] rg_prod_par7_2;
  wire [105 : 0] rg_prod_par7_2$D_IN;
  wire rg_prod_par7_2$EN;

  // register rg_prod_par8_1
  reg [105 : 0] rg_prod_par8_1;
  wire [105 : 0] rg_prod_par8_1$D_IN;
  wire rg_prod_par8_1$EN;

  // register rg_prod_par8_2
  reg [105 : 0] rg_prod_par8_2;
  wire [105 : 0] rg_prod_par8_2$D_IN;
  wire rg_prod_par8_2$EN;

  // register rg_sum_exp
  reg [11 : 0] rg_sum_exp;
  wire [11 : 0] rg_sum_exp$D_IN;
  wire rg_sum_exp$EN;

  // register rg_unnorm_exp0
  reg [11 : 0] rg_unnorm_exp0;
  wire [11 : 0] rg_unnorm_exp0$D_IN;
  wire rg_unnorm_exp0$EN;

  // register rg_unnorm_exp1
  reg [11 : 0] rg_unnorm_exp1;
  wire [11 : 0] rg_unnorm_exp1$D_IN;
  wire rg_unnorm_exp1$EN;

  // register rg_unnorm_mp
  reg [105 : 0] rg_unnorm_mp;
  wire [105 : 0] rg_unnorm_mp$D_IN;
  wire rg_unnorm_mp$EN;

  // ports of submodule valid_receive
  wire valid_receive$CLR,
       valid_receive$DEQ,
       valid_receive$D_IN,
       valid_receive$D_OUT,
       valid_receive$EMPTY_N,
       valid_receive$ENQ,
       valid_receive$FULL_N;

  // ports of submodule valid_send
  wire valid_send$CLR,
       valid_send$DEQ,
       valid_send$D_IN,
       valid_send$D_OUT,
       valid_send$EMPTY_N,
       valid_send$ENQ,
       valid_send$FULL_N;

  // ports of submodule valid_stage1
  wire valid_stage1$CLR,
       valid_stage1$DEQ,
       valid_stage1$D_IN,
       valid_stage1$D_OUT,
       valid_stage1$EMPTY_N,
       valid_stage1$ENQ,
       valid_stage1$FULL_N;

  // ports of submodule valid_stage2
  wire valid_stage2$CLR,
       valid_stage2$DEQ,
       valid_stage2$D_IN,
       valid_stage2$D_OUT,
       valid_stage2$EMPTY_N,
       valid_stage2$ENQ,
       valid_stage2$FULL_N;

  // ports of submodule valid_stage3
  wire valid_stage3$CLR,
       valid_stage3$DEQ,
       valid_stage3$D_IN,
       valid_stage3$D_OUT,
       valid_stage3$EMPTY_N,
       valid_stage3$ENQ,
       valid_stage3$FULL_N;

  // ports of submodule valid_stage4
  wire valid_stage4$CLR,
       valid_stage4$DEQ,
       valid_stage4$D_IN,
       valid_stage4$D_OUT,
       valid_stage4$EMPTY_N,
       valid_stage4$ENQ,
       valid_stage4$FULL_N;

  // remaining internal signals
  wire [105 : 0] x__h4574, x__h4613, x__h4652, x__h4793;
  wire [59 : 0] ans__h1893,
		ans__h2148,
		ans__h2380,
		ans__h2612,
		ans__h2844,
		ans__h3076,
		ans__h3308,
		ans__h3540,
		ans__h3772,
		op1__h1880,
		res1__h1887,
		res1__h2142,
		res1__h2374,
		res1__h2606,
		res1__h2838,
		res1__h3070,
		res1__h3302,
		res1__h3534,
		res1__h3766,
		res2__h1888,
		res2__h2143,
		res2__h2375,
		res2__h2607,
		res2__h2839,
		res2__h3071,
		res2__h3303,
		res2__h3535,
		res2__h3767,
		res3__h1889,
		res3__h2144,
		res3__h2376,
		res3__h2608,
		res3__h2840,
		res3__h3072,
		res3__h3304,
		res3__h3536,
		res3__h3768,
		res4__h1890,
		res4__h2145,
		res4__h2377,
		res4__h2609,
		res4__h2841,
		res4__h3073,
		res4__h3305,
		res4__h3537,
		res4__h3769,
		res5__h1891,
		res5__h2146,
		res5__h2378,
		res5__h2610,
		res5__h2842,
		res5__h3074,
		res5__h3306,
		res5__h3538,
		res6__h1892,
		res6__h2147,
		res6__h2379,
		res6__h2611,
		res6__h2843,
		res6__h3075,
		res6__h3307,
		res6__h3539,
		x__h1912,
		x__h1913,
		x__h1914,
		x__h1915,
		x__h2167,
		x__h2168,
		x__h2169,
		x__h2170,
		x__h2399,
		x__h2400,
		x__h2401,
		x__h2402,
		x__h2631,
		x__h2632,
		x__h2633,
		x__h2634,
		x__h2863,
		x__h2864,
		x__h2865,
		x__h2866,
		x__h3095,
		x__h3096,
		x__h3097,
		x__h3098,
		x__h3327,
		x__h3328,
		x__h3329,
		x__h3330,
		x__h3559,
		x__h3560,
		x__h3561,
		x__h3562,
		x__h3792,
		x__h3793,
		x__h3794;
  wire [11 : 0] x__h4073, y__h4074;

  // action method send
  assign RDY_send = valid_send$FULL_N ;

  // actionvalue method receive
  assign receive = { r_sgn_fin, rg_fin_exp[10:0], rg_fin_m } ;
  assign RDY_receive = valid_receive$EMPTY_N && valid_receive$D_OUT ;

  // submodule valid_receive
  FIFOL1 #(.width(32'd1)) valid_receive(.RST(RST_N),
					.CLK(CLK),
					.D_IN(valid_receive$D_IN),
					.ENQ(valid_receive$ENQ),
					.DEQ(valid_receive$DEQ),
					.CLR(valid_receive$CLR),
					.D_OUT(valid_receive$D_OUT),
					.FULL_N(valid_receive$FULL_N),
					.EMPTY_N(valid_receive$EMPTY_N));

  // submodule valid_send
  FIFOL1 #(.width(32'd1)) valid_send(.RST(RST_N),
				     .CLK(CLK),
				     .D_IN(valid_send$D_IN),
				     .ENQ(valid_send$ENQ),
				     .DEQ(valid_send$DEQ),
				     .CLR(valid_send$CLR),
				     .D_OUT(valid_send$D_OUT),
				     .FULL_N(valid_send$FULL_N),
				     .EMPTY_N(valid_send$EMPTY_N));

  // submodule valid_stage1
  FIFOL1 #(.width(32'd1)) valid_stage1(.RST(RST_N),
				       .CLK(CLK),
				       .D_IN(valid_stage1$D_IN),
				       .ENQ(valid_stage1$ENQ),
				       .DEQ(valid_stage1$DEQ),
				       .CLR(valid_stage1$CLR),
				       .D_OUT(valid_stage1$D_OUT),
				       .FULL_N(valid_stage1$FULL_N),
				       .EMPTY_N(valid_stage1$EMPTY_N));

  // submodule valid_stage2
  FIFOL1 #(.width(32'd1)) valid_stage2(.RST(RST_N),
				       .CLK(CLK),
				       .D_IN(valid_stage2$D_IN),
				       .ENQ(valid_stage2$ENQ),
				       .DEQ(valid_stage2$DEQ),
				       .CLR(valid_stage2$CLR),
				       .D_OUT(valid_stage2$D_OUT),
				       .FULL_N(valid_stage2$FULL_N),
				       .EMPTY_N(valid_stage2$EMPTY_N));

  // submodule valid_stage3
  FIFOL1 #(.width(32'd1)) valid_stage3(.RST(RST_N),
				       .CLK(CLK),
				       .D_IN(valid_stage3$D_IN),
				       .ENQ(valid_stage3$ENQ),
				       .DEQ(valid_stage3$DEQ),
				       .CLR(valid_stage3$CLR),
				       .D_OUT(valid_stage3$D_OUT),
				       .FULL_N(valid_stage3$FULL_N),
				       .EMPTY_N(valid_stage3$EMPTY_N));

  // submodule valid_stage4
  FIFOL1 #(.width(32'd1)) valid_stage4(.RST(RST_N),
				       .CLK(CLK),
				       .D_IN(valid_stage4$D_IN),
				       .ENQ(valid_stage4$ENQ),
				       .DEQ(valid_stage4$DEQ),
				       .CLR(valid_stage4$CLR),
				       .D_OUT(valid_stage4$D_OUT),
				       .FULL_N(valid_stage4$FULL_N),
				       .EMPTY_N(valid_stage4$EMPTY_N));

  // register d_copy1
  assign d_copy1$D_IN = send_data_in ;
  assign d_copy1$EN = EN_send ;

  // register r_sgn_1
  assign r_sgn_1$D_IN = d_copy1[127] != d_copy1[63] ;
  assign r_sgn_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register r_sgn_2
  assign r_sgn_2$D_IN = r_sgn_1 ;
  assign r_sgn_2$EN =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;

  // register r_sgn_3
  assign r_sgn_3$D_IN = r_sgn_2 ;
  assign r_sgn_3$EN =
	     valid_stage2$EMPTY_N && valid_stage3$FULL_N &&
	     valid_stage2$D_OUT ;

  // register r_sgn_4
  assign r_sgn_4$D_IN = r_sgn_3 ;
  assign r_sgn_4$EN =
	     valid_stage3$EMPTY_N && valid_stage4$FULL_N &&
	     valid_stage3$D_OUT ;

  // register r_sgn_fin
  assign r_sgn_fin$D_IN = r_sgn_4 ;
  assign r_sgn_fin$EN =
	     valid_stage4$EMPTY_N && valid_receive$FULL_N &&
	     valid_stage4$D_OUT ;

  // register rg_exp1_1
  assign rg_exp1_1$D_IN = d_copy1[126:116] ;
  assign rg_exp1_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register rg_exp2_1
  assign rg_exp2_1$D_IN = d_copy1[62:52] ;
  assign rg_exp2_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register rg_fin_exp
  assign rg_fin_exp$D_IN =
	     rg_unnorm_mp[105] ? rg_unnorm_exp1 + 12'd1 : rg_unnorm_exp1 ;
  assign rg_fin_exp$EN =
	     valid_stage4$EMPTY_N && valid_receive$FULL_N &&
	     valid_stage4$D_OUT ;

  // register rg_fin_m
  assign rg_fin_m$D_IN =
	     rg_unnorm_mp[105] ? rg_unnorm_mp[104:53] : rg_unnorm_mp[103:52] ;
  assign rg_fin_m$EN =
	     valid_stage4$EMPTY_N && valid_receive$FULL_N &&
	     valid_stage4$D_OUT ;

  // register rg_prod_par012
  assign rg_prod_par012$D_IN = x__h4574 + rg_prod_par2_2 ;
  assign rg_prod_par012$EN =
	     valid_stage2$EMPTY_N && valid_stage3$FULL_N &&
	     valid_stage2$D_OUT ;

  // register rg_prod_par0_1
  assign rg_prod_par0_1$D_IN = { 46'd0, ans__h1893 } ;
  assign rg_prod_par0_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register rg_prod_par0_2
  assign rg_prod_par0_2$D_IN = rg_prod_par0_1 ;
  assign rg_prod_par0_2$EN =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;

  // register rg_prod_par1_1
  assign rg_prod_par1_1$D_IN = { 46'd0, ans__h2148 } ;
  assign rg_prod_par1_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register rg_prod_par1_2
  assign rg_prod_par1_2$D_IN = { rg_prod_par1_1[99:0], 6'd0 } ;
  assign rg_prod_par1_2$EN =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;

  // register rg_prod_par2_1
  assign rg_prod_par2_1$D_IN = { 46'd0, ans__h2380 } ;
  assign rg_prod_par2_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register rg_prod_par2_2
  assign rg_prod_par2_2$D_IN = { rg_prod_par2_1[93:0], 12'd0 } ;
  assign rg_prod_par2_2$EN =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;

  // register rg_prod_par345
  assign rg_prod_par345$D_IN = x__h4613 + rg_prod_par5_2 ;
  assign rg_prod_par345$EN =
	     valid_stage2$EMPTY_N && valid_stage3$FULL_N &&
	     valid_stage2$D_OUT ;

  // register rg_prod_par3_1
  assign rg_prod_par3_1$D_IN = { 46'd0, ans__h2612 } ;
  assign rg_prod_par3_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register rg_prod_par3_2
  assign rg_prod_par3_2$D_IN = { rg_prod_par3_1[87:0], 18'd0 } ;
  assign rg_prod_par3_2$EN =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;

  // register rg_prod_par4_1
  assign rg_prod_par4_1$D_IN = { 46'd0, ans__h2844 } ;
  assign rg_prod_par4_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register rg_prod_par4_2
  assign rg_prod_par4_2$D_IN = { rg_prod_par4_1[81:0], 24'd0 } ;
  assign rg_prod_par4_2$EN =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;

  // register rg_prod_par5_1
  assign rg_prod_par5_1$D_IN = { 46'd0, ans__h3076 } ;
  assign rg_prod_par5_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register rg_prod_par5_2
  assign rg_prod_par5_2$D_IN = { rg_prod_par5_1[75:0], 30'd0 } ;
  assign rg_prod_par5_2$EN =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;

  // register rg_prod_par678
  assign rg_prod_par678$D_IN = x__h4652 + rg_prod_par8_2 ;
  assign rg_prod_par678$EN =
	     valid_stage2$EMPTY_N && valid_stage3$FULL_N &&
	     valid_stage2$D_OUT ;

  // register rg_prod_par6_1
  assign rg_prod_par6_1$D_IN = { 46'd0, ans__h3308 } ;
  assign rg_prod_par6_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register rg_prod_par6_2
  assign rg_prod_par6_2$D_IN = { rg_prod_par6_1[69:0], 36'd0 } ;
  assign rg_prod_par6_2$EN =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;

  // register rg_prod_par7_1
  assign rg_prod_par7_1$D_IN = { 46'd0, ans__h3540 } ;
  assign rg_prod_par7_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register rg_prod_par7_2
  assign rg_prod_par7_2$D_IN = { rg_prod_par7_1[63:0], 42'd0 } ;
  assign rg_prod_par7_2$EN =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;

  // register rg_prod_par8_1
  assign rg_prod_par8_1$D_IN = { 46'd0, ans__h3772 } ;
  assign rg_prod_par8_1$EN =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;

  // register rg_prod_par8_2
  assign rg_prod_par8_2$D_IN = { rg_prod_par8_1[57:0], 48'd0 } ;
  assign rg_prod_par8_2$EN =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;

  // register rg_sum_exp
  assign rg_sum_exp$D_IN = x__h4073 + y__h4074 ;
  assign rg_sum_exp$EN =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;

  // register rg_unnorm_exp0
  assign rg_unnorm_exp0$D_IN = rg_sum_exp + 12'd3073 ;
  assign rg_unnorm_exp0$EN =
	     valid_stage2$EMPTY_N && valid_stage3$FULL_N &&
	     valid_stage2$D_OUT ;

  // register rg_unnorm_exp1
  assign rg_unnorm_exp1$D_IN = rg_unnorm_exp0 ;
  assign rg_unnorm_exp1$EN =
	     valid_stage3$EMPTY_N && valid_stage4$FULL_N &&
	     valid_stage3$D_OUT ;

  // register rg_unnorm_mp
  assign rg_unnorm_mp$D_IN = x__h4793 + rg_prod_par678 ;
  assign rg_unnorm_mp$EN =
	     valid_stage3$EMPTY_N && valid_stage4$FULL_N &&
	     valid_stage3$D_OUT ;

  // submodule valid_receive
  assign valid_receive$D_IN = 1'd1 ;
  assign valid_receive$ENQ =
	     valid_stage4$EMPTY_N && valid_receive$FULL_N &&
	     valid_stage4$D_OUT ;
  assign valid_receive$DEQ = EN_receive ;
  assign valid_receive$CLR = 1'b0 ;

  // submodule valid_send
  assign valid_send$D_IN = 1'd1 ;
  assign valid_send$ENQ = EN_send ;
  assign valid_send$DEQ =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;
  assign valid_send$CLR = 1'b0 ;

  // submodule valid_stage1
  assign valid_stage1$D_IN = 1'd1 ;
  assign valid_stage1$ENQ =
	     valid_send$EMPTY_N && valid_stage1$FULL_N && valid_send$D_OUT ;
  assign valid_stage1$DEQ =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;
  assign valid_stage1$CLR = 1'b0 ;

  // submodule valid_stage2
  assign valid_stage2$D_IN = 1'd1 ;
  assign valid_stage2$ENQ =
	     valid_stage1$EMPTY_N && valid_stage2$FULL_N &&
	     valid_stage1$D_OUT ;
  assign valid_stage2$DEQ =
	     valid_stage2$EMPTY_N && valid_stage3$FULL_N &&
	     valid_stage2$D_OUT ;
  assign valid_stage2$CLR = 1'b0 ;

  // submodule valid_stage3
  assign valid_stage3$D_IN = 1'd1 ;
  assign valid_stage3$ENQ =
	     valid_stage2$EMPTY_N && valid_stage3$FULL_N &&
	     valid_stage2$D_OUT ;
  assign valid_stage3$DEQ =
	     valid_stage3$EMPTY_N && valid_stage4$FULL_N &&
	     valid_stage3$D_OUT ;
  assign valid_stage3$CLR = 1'b0 ;

  // submodule valid_stage4
  assign valid_stage4$D_IN = 1'd1 ;
  assign valid_stage4$ENQ =
	     valid_stage3$EMPTY_N && valid_stage4$FULL_N &&
	     valid_stage3$D_OUT ;
  assign valid_stage4$DEQ =
	     valid_stage4$EMPTY_N && valid_receive$FULL_N &&
	     valid_stage4$D_OUT ;
  assign valid_stage4$CLR = 1'b0 ;

  // remaining internal signals
  assign ans__h1893 = x__h1912 + res6__h1892 ;
  assign ans__h2148 = x__h2167 + res6__h2147 ;
  assign ans__h2380 = x__h2399 + res6__h2379 ;
  assign ans__h2612 = x__h2631 + res6__h2611 ;
  assign ans__h2844 = x__h2863 + res6__h2843 ;
  assign ans__h3076 = x__h3095 + res6__h3075 ;
  assign ans__h3308 = x__h3327 + res6__h3307 ;
  assign ans__h3540 = x__h3559 + res6__h3539 ;
  assign ans__h3772 = x__h3792 + { 4'd1, d_copy1[115:64], 4'd0 } ;
  assign op1__h1880 = { 8'd1, d_copy1[115:64] } ;
  assign res1__h1887 = d_copy1[0] ? op1__h1880 : 60'd0 ;
  assign res1__h2142 = d_copy1[6] ? op1__h1880 : 60'd0 ;
  assign res1__h2374 = d_copy1[12] ? op1__h1880 : 60'd0 ;
  assign res1__h2606 = d_copy1[18] ? op1__h1880 : 60'd0 ;
  assign res1__h2838 = d_copy1[24] ? op1__h1880 : 60'd0 ;
  assign res1__h3070 = d_copy1[30] ? op1__h1880 : 60'd0 ;
  assign res1__h3302 = d_copy1[36] ? op1__h1880 : 60'd0 ;
  assign res1__h3534 = d_copy1[42] ? op1__h1880 : 60'd0 ;
  assign res1__h3766 = d_copy1[48] ? op1__h1880 : 60'd0 ;
  assign res2__h1888 = d_copy1[1] ? { 7'd1, d_copy1[115:64], 1'd0 } : 60'd0 ;
  assign res2__h2143 = d_copy1[7] ? { 7'd1, d_copy1[115:64], 1'd0 } : 60'd0 ;
  assign res2__h2375 = d_copy1[13] ? { 7'd1, d_copy1[115:64], 1'd0 } : 60'd0 ;
  assign res2__h2607 = d_copy1[19] ? { 7'd1, d_copy1[115:64], 1'd0 } : 60'd0 ;
  assign res2__h2839 = d_copy1[25] ? { 7'd1, d_copy1[115:64], 1'd0 } : 60'd0 ;
  assign res2__h3071 = d_copy1[31] ? { 7'd1, d_copy1[115:64], 1'd0 } : 60'd0 ;
  assign res2__h3303 = d_copy1[37] ? { 7'd1, d_copy1[115:64], 1'd0 } : 60'd0 ;
  assign res2__h3535 = d_copy1[43] ? { 7'd1, d_copy1[115:64], 1'd0 } : 60'd0 ;
  assign res2__h3767 = d_copy1[49] ? { 7'd1, d_copy1[115:64], 1'd0 } : 60'd0 ;
  assign res3__h1889 = d_copy1[2] ? { 6'd1, d_copy1[115:64], 2'd0 } : 60'd0 ;
  assign res3__h2144 = d_copy1[8] ? { 6'd1, d_copy1[115:64], 2'd0 } : 60'd0 ;
  assign res3__h2376 = d_copy1[14] ? { 6'd1, d_copy1[115:64], 2'd0 } : 60'd0 ;
  assign res3__h2608 = d_copy1[20] ? { 6'd1, d_copy1[115:64], 2'd0 } : 60'd0 ;
  assign res3__h2840 = d_copy1[26] ? { 6'd1, d_copy1[115:64], 2'd0 } : 60'd0 ;
  assign res3__h3072 = d_copy1[32] ? { 6'd1, d_copy1[115:64], 2'd0 } : 60'd0 ;
  assign res3__h3304 = d_copy1[38] ? { 6'd1, d_copy1[115:64], 2'd0 } : 60'd0 ;
  assign res3__h3536 = d_copy1[44] ? { 6'd1, d_copy1[115:64], 2'd0 } : 60'd0 ;
  assign res3__h3768 = d_copy1[50] ? { 6'd1, d_copy1[115:64], 2'd0 } : 60'd0 ;
  assign res4__h1890 = d_copy1[3] ? { 5'd1, d_copy1[115:64], 3'd0 } : 60'd0 ;
  assign res4__h2145 = d_copy1[9] ? { 5'd1, d_copy1[115:64], 3'd0 } : 60'd0 ;
  assign res4__h2377 = d_copy1[15] ? { 5'd1, d_copy1[115:64], 3'd0 } : 60'd0 ;
  assign res4__h2609 = d_copy1[21] ? { 5'd1, d_copy1[115:64], 3'd0 } : 60'd0 ;
  assign res4__h2841 = d_copy1[27] ? { 5'd1, d_copy1[115:64], 3'd0 } : 60'd0 ;
  assign res4__h3073 = d_copy1[33] ? { 5'd1, d_copy1[115:64], 3'd0 } : 60'd0 ;
  assign res4__h3305 = d_copy1[39] ? { 5'd1, d_copy1[115:64], 3'd0 } : 60'd0 ;
  assign res4__h3537 = d_copy1[45] ? { 5'd1, d_copy1[115:64], 3'd0 } : 60'd0 ;
  assign res4__h3769 = d_copy1[51] ? { 5'd1, d_copy1[115:64], 3'd0 } : 60'd0 ;
  assign res5__h1891 = d_copy1[4] ? { 4'd1, d_copy1[115:64], 4'd0 } : 60'd0 ;
  assign res5__h2146 = d_copy1[10] ? { 4'd1, d_copy1[115:64], 4'd0 } : 60'd0 ;
  assign res5__h2378 = d_copy1[16] ? { 4'd1, d_copy1[115:64], 4'd0 } : 60'd0 ;
  assign res5__h2610 = d_copy1[22] ? { 4'd1, d_copy1[115:64], 4'd0 } : 60'd0 ;
  assign res5__h2842 = d_copy1[28] ? { 4'd1, d_copy1[115:64], 4'd0 } : 60'd0 ;
  assign res5__h3074 = d_copy1[34] ? { 4'd1, d_copy1[115:64], 4'd0 } : 60'd0 ;
  assign res5__h3306 = d_copy1[40] ? { 4'd1, d_copy1[115:64], 4'd0 } : 60'd0 ;
  assign res5__h3538 = d_copy1[46] ? { 4'd1, d_copy1[115:64], 4'd0 } : 60'd0 ;
  assign res6__h1892 = d_copy1[5] ? { 3'd1, d_copy1[115:64], 5'd0 } : 60'd0 ;
  assign res6__h2147 = d_copy1[11] ? { 3'd1, d_copy1[115:64], 5'd0 } : 60'd0 ;
  assign res6__h2379 = d_copy1[17] ? { 3'd1, d_copy1[115:64], 5'd0 } : 60'd0 ;
  assign res6__h2611 = d_copy1[23] ? { 3'd1, d_copy1[115:64], 5'd0 } : 60'd0 ;
  assign res6__h2843 = d_copy1[29] ? { 3'd1, d_copy1[115:64], 5'd0 } : 60'd0 ;
  assign res6__h3075 = d_copy1[35] ? { 3'd1, d_copy1[115:64], 5'd0 } : 60'd0 ;
  assign res6__h3307 = d_copy1[41] ? { 3'd1, d_copy1[115:64], 5'd0 } : 60'd0 ;
  assign res6__h3539 = d_copy1[47] ? { 3'd1, d_copy1[115:64], 5'd0 } : 60'd0 ;
  assign x__h1912 = x__h1913 + res5__h1891 ;
  assign x__h1913 = x__h1914 + res4__h1890 ;
  assign x__h1914 = x__h1915 + res3__h1889 ;
  assign x__h1915 = res1__h1887 + res2__h1888 ;
  assign x__h2167 = x__h2168 + res5__h2146 ;
  assign x__h2168 = x__h2169 + res4__h2145 ;
  assign x__h2169 = x__h2170 + res3__h2144 ;
  assign x__h2170 = res1__h2142 + res2__h2143 ;
  assign x__h2399 = x__h2400 + res5__h2378 ;
  assign x__h2400 = x__h2401 + res4__h2377 ;
  assign x__h2401 = x__h2402 + res3__h2376 ;
  assign x__h2402 = res1__h2374 + res2__h2375 ;
  assign x__h2631 = x__h2632 + res5__h2610 ;
  assign x__h2632 = x__h2633 + res4__h2609 ;
  assign x__h2633 = x__h2634 + res3__h2608 ;
  assign x__h2634 = res1__h2606 + res2__h2607 ;
  assign x__h2863 = x__h2864 + res5__h2842 ;
  assign x__h2864 = x__h2865 + res4__h2841 ;
  assign x__h2865 = x__h2866 + res3__h2840 ;
  assign x__h2866 = res1__h2838 + res2__h2839 ;
  assign x__h3095 = x__h3096 + res5__h3074 ;
  assign x__h3096 = x__h3097 + res4__h3073 ;
  assign x__h3097 = x__h3098 + res3__h3072 ;
  assign x__h3098 = res1__h3070 + res2__h3071 ;
  assign x__h3327 = x__h3328 + res5__h3306 ;
  assign x__h3328 = x__h3329 + res4__h3305 ;
  assign x__h3329 = x__h3330 + res3__h3304 ;
  assign x__h3330 = res1__h3302 + res2__h3303 ;
  assign x__h3559 = x__h3560 + res5__h3538 ;
  assign x__h3560 = x__h3561 + res4__h3537 ;
  assign x__h3561 = x__h3562 + res3__h3536 ;
  assign x__h3562 = res1__h3534 + res2__h3535 ;
  assign x__h3792 = x__h3793 + res4__h3769 ;
  assign x__h3793 = x__h3794 + res3__h3768 ;
  assign x__h3794 = res1__h3766 + res2__h3767 ;
  assign x__h4073 = { 1'd0, rg_exp1_1 } ;
  assign x__h4574 = rg_prod_par0_2 + rg_prod_par1_2 ;
  assign x__h4613 = rg_prod_par3_2 + rg_prod_par4_2 ;
  assign x__h4652 = rg_prod_par6_2 + rg_prod_par7_2 ;
  assign x__h4793 = rg_prod_par012 + rg_prod_par345 ;
  assign y__h4074 = { 1'd0, rg_exp2_1 } ;

  // handling of inlined registers

  always@(posedge CLK)
  begin
    if (RST_N == `BSV_RESET_VALUE)
      begin
        d_copy1 <= `BSV_ASSIGNMENT_DELAY 128'd0;
	r_sgn_1 <= `BSV_ASSIGNMENT_DELAY 1'd0;
	r_sgn_2 <= `BSV_ASSIGNMENT_DELAY 1'd0;
	r_sgn_3 <= `BSV_ASSIGNMENT_DELAY 1'd0;
	r_sgn_4 <= `BSV_ASSIGNMENT_DELAY 1'd0;
	r_sgn_fin <= `BSV_ASSIGNMENT_DELAY 1'd0;
	rg_exp1_1 <= `BSV_ASSIGNMENT_DELAY 11'd0;
	rg_exp2_1 <= `BSV_ASSIGNMENT_DELAY 11'd0;
	rg_fin_exp <= `BSV_ASSIGNMENT_DELAY 12'd0;
	rg_fin_m <= `BSV_ASSIGNMENT_DELAY 52'd0;
	rg_prod_par012 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par0_1 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par0_2 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par1_1 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par1_2 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par2_1 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par2_2 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par345 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par3_1 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par3_2 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par4_1 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par4_2 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par5_1 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par5_2 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par678 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par6_1 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par6_2 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par7_1 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par7_2 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par8_1 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_prod_par8_2 <= `BSV_ASSIGNMENT_DELAY 106'd0;
	rg_sum_exp <= `BSV_ASSIGNMENT_DELAY 12'd0;
	rg_unnorm_exp0 <= `BSV_ASSIGNMENT_DELAY 12'd0;
	rg_unnorm_exp1 <= `BSV_ASSIGNMENT_DELAY 12'd0;
	rg_unnorm_mp <= `BSV_ASSIGNMENT_DELAY 106'd0;
      end
    else
      begin
        if (d_copy1$EN) d_copy1 <= `BSV_ASSIGNMENT_DELAY d_copy1$D_IN;
	if (r_sgn_1$EN) r_sgn_1 <= `BSV_ASSIGNMENT_DELAY r_sgn_1$D_IN;
	if (r_sgn_2$EN) r_sgn_2 <= `BSV_ASSIGNMENT_DELAY r_sgn_2$D_IN;
	if (r_sgn_3$EN) r_sgn_3 <= `BSV_ASSIGNMENT_DELAY r_sgn_3$D_IN;
	if (r_sgn_4$EN) r_sgn_4 <= `BSV_ASSIGNMENT_DELAY r_sgn_4$D_IN;
	if (r_sgn_fin$EN) r_sgn_fin <= `BSV_ASSIGNMENT_DELAY r_sgn_fin$D_IN;
	if (rg_exp1_1$EN) rg_exp1_1 <= `BSV_ASSIGNMENT_DELAY rg_exp1_1$D_IN;
	if (rg_exp2_1$EN) rg_exp2_1 <= `BSV_ASSIGNMENT_DELAY rg_exp2_1$D_IN;
	if (rg_fin_exp$EN)
	  rg_fin_exp <= `BSV_ASSIGNMENT_DELAY rg_fin_exp$D_IN;
	if (rg_fin_m$EN) rg_fin_m <= `BSV_ASSIGNMENT_DELAY rg_fin_m$D_IN;
	if (rg_prod_par012$EN)
	  rg_prod_par012 <= `BSV_ASSIGNMENT_DELAY rg_prod_par012$D_IN;
	if (rg_prod_par0_1$EN)
	  rg_prod_par0_1 <= `BSV_ASSIGNMENT_DELAY rg_prod_par0_1$D_IN;
	if (rg_prod_par0_2$EN)
	  rg_prod_par0_2 <= `BSV_ASSIGNMENT_DELAY rg_prod_par0_2$D_IN;
	if (rg_prod_par1_1$EN)
	  rg_prod_par1_1 <= `BSV_ASSIGNMENT_DELAY rg_prod_par1_1$D_IN;
	if (rg_prod_par1_2$EN)
	  rg_prod_par1_2 <= `BSV_ASSIGNMENT_DELAY rg_prod_par1_2$D_IN;
	if (rg_prod_par2_1$EN)
	  rg_prod_par2_1 <= `BSV_ASSIGNMENT_DELAY rg_prod_par2_1$D_IN;
	if (rg_prod_par2_2$EN)
	  rg_prod_par2_2 <= `BSV_ASSIGNMENT_DELAY rg_prod_par2_2$D_IN;
	if (rg_prod_par345$EN)
	  rg_prod_par345 <= `BSV_ASSIGNMENT_DELAY rg_prod_par345$D_IN;
	if (rg_prod_par3_1$EN)
	  rg_prod_par3_1 <= `BSV_ASSIGNMENT_DELAY rg_prod_par3_1$D_IN;
	if (rg_prod_par3_2$EN)
	  rg_prod_par3_2 <= `BSV_ASSIGNMENT_DELAY rg_prod_par3_2$D_IN;
	if (rg_prod_par4_1$EN)
	  rg_prod_par4_1 <= `BSV_ASSIGNMENT_DELAY rg_prod_par4_1$D_IN;
	if (rg_prod_par4_2$EN)
	  rg_prod_par4_2 <= `BSV_ASSIGNMENT_DELAY rg_prod_par4_2$D_IN;
	if (rg_prod_par5_1$EN)
	  rg_prod_par5_1 <= `BSV_ASSIGNMENT_DELAY rg_prod_par5_1$D_IN;
	if (rg_prod_par5_2$EN)
	  rg_prod_par5_2 <= `BSV_ASSIGNMENT_DELAY rg_prod_par5_2$D_IN;
	if (rg_prod_par678$EN)
	  rg_prod_par678 <= `BSV_ASSIGNMENT_DELAY rg_prod_par678$D_IN;
	if (rg_prod_par6_1$EN)
	  rg_prod_par6_1 <= `BSV_ASSIGNMENT_DELAY rg_prod_par6_1$D_IN;
	if (rg_prod_par6_2$EN)
	  rg_prod_par6_2 <= `BSV_ASSIGNMENT_DELAY rg_prod_par6_2$D_IN;
	if (rg_prod_par7_1$EN)
	  rg_prod_par7_1 <= `BSV_ASSIGNMENT_DELAY rg_prod_par7_1$D_IN;
	if (rg_prod_par7_2$EN)
	  rg_prod_par7_2 <= `BSV_ASSIGNMENT_DELAY rg_prod_par7_2$D_IN;
	if (rg_prod_par8_1$EN)
	  rg_prod_par8_1 <= `BSV_ASSIGNMENT_DELAY rg_prod_par8_1$D_IN;
	if (rg_prod_par8_2$EN)
	  rg_prod_par8_2 <= `BSV_ASSIGNMENT_DELAY rg_prod_par8_2$D_IN;
	if (rg_sum_exp$EN)
	  rg_sum_exp <= `BSV_ASSIGNMENT_DELAY rg_sum_exp$D_IN;
	if (rg_unnorm_exp0$EN)
	  rg_unnorm_exp0 <= `BSV_ASSIGNMENT_DELAY rg_unnorm_exp0$D_IN;
	if (rg_unnorm_exp1$EN)
	  rg_unnorm_exp1 <= `BSV_ASSIGNMENT_DELAY rg_unnorm_exp1$D_IN;
	if (rg_unnorm_mp$EN)
	  rg_unnorm_mp <= `BSV_ASSIGNMENT_DELAY rg_unnorm_mp$D_IN;
      end
  end

  // synopsys translate_off
  `ifdef BSV_NO_INITIAL_BLOCKS
  `else // not BSV_NO_INITIAL_BLOCKS
  initial
  begin
    d_copy1 = 128'hAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA;
    r_sgn_1 = 1'h0;
    r_sgn_2 = 1'h0;
    r_sgn_3 = 1'h0;
    r_sgn_4 = 1'h0;
    r_sgn_fin = 1'h0;
    rg_exp1_1 = 11'h2AA;
    rg_exp2_1 = 11'h2AA;
    rg_fin_exp = 12'hAAA;
    rg_fin_m = 52'hAAAAAAAAAAAAA;
    rg_prod_par012 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par0_1 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par0_2 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par1_1 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par1_2 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par2_1 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par2_2 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par345 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par3_1 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par3_2 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par4_1 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par4_2 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par5_1 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par5_2 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par678 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par6_1 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par6_2 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par7_1 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par7_2 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par8_1 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_prod_par8_2 = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
    rg_sum_exp = 12'hAAA;
    rg_unnorm_exp0 = 12'hAAA;
    rg_unnorm_exp1 = 12'hAAA;
    rg_unnorm_mp = 106'h2AAAAAAAAAAAAAAAAAAAAAAAAAA;
  end
  `endif // BSV_NO_INITIAL_BLOCKS
  // synopsys translate_on
endmodule  // mkPDpMul
