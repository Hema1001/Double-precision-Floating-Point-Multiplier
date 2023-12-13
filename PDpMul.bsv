/*
  Filename      : PDpMul.bsv
  Description   : Pipelined Double precision Floating point multiplier
*/

package PDpMul;

/*imports*/
import FixedPoint    ::*;
import FloatingPoint ::*;
import FIFO          ::*;

/*double precision Multiplier Interface*/
interface Ifc_fpu_multiplier_dp;                                                
  method Action send(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52)) data_in);
  method ActionValue#(FloatingPoint#(11,52)) receive();
endinterface

/////////////////////////////////////////testbench/////////////////////////////////////////////////////////
(*synthesize*)
module mkTest(Empty);

   
   // Reg#(int) y1 <- mkReg ('h00);

    Ifc_fpu_multiplier_dp pipe <- mkPDpMul ;
	
    Reg#(Bit#(8)) rg_cycle <- mkReg(0);

//Testcases

    rule rl_send0(rg_cycle==0); //sending first set of inputs in first cycle 
      FloatingPoint#(11, 52) f,g;

      f.sign = True;
      f.exp = 11'h401;
      f.sfd = 52'h4000000000000;
      
      g.sign = False;
      g.exp = 11'h400;
      g.sfd = 52'h1;


      pipe.send(tuple2(f,g));
      $display("cycle:",rg_cycle," ",fshow(f),fshow(g));

      //`logLevel( tb, 1, $format("TB sqrt: cycle %d: send operands ", rg_cycle, fshow(f)))
    endrule


    rule rl_send1(rg_cycle==1); //sending 2nd set of inputs in 2nd cycle 
      FloatingPoint#(11, 52) f,g;

     // Floating-point number f
	f.sign = True;
	f.exp = 11'h3FF; // Exponent for 1.0
	f.sfd = 52'h23456789ABCDE; // Random mantissa

	// Floating-point number g
	g.sign = False;
	g.exp = 11'h400; // Exponent for 2.0
	g.sfd = 52'h8000000000000; // Negative mantissa for -2.0


      pipe.send(tuple2(f,g));
      $display("cycle:",rg_cycle," ",fshow(f),fshow(g));

      //`logLevel( tb, 1, $format("TB sqrt: cycle %d: send operands ", rg_cycle, fshow(f)))
    endrule

    rule rl_send2(rg_cycle==2); //sending 3rd set of inputs in 3rd cycle 
      FloatingPoint#(11, 52) f,g;

     // Floating-point number f (subnormal)
	f.sign = True;
	f.exp = 11'h000; // Exponent for subnormal numbers
	f.sfd = 52'h0000000000001; // Non-zero subnormal mantissa

	// Floating-point number g (subnormal)
	g.sign = True;
	g.exp = 11'h000; // Exponent for subnormal numbers
	g.sfd = 52'h000123456789A; // Non-zero subnormal mantissa


      pipe.send(tuple2(f,g));
      $display("cycle:",rg_cycle," ",fshow(f),fshow(g));

      //`logLevel( tb, 1, $format("TB sqrt: cycle %d: send operands ", rg_cycle, fshow(f)))
    endrule


    rule rl_send3(rg_cycle==3); //sending 4th set of inputs in 4th cycle 
      FloatingPoint#(11, 52) f,g;


	// Floating-point number f
	f.sign = True;
	f.exp = 11'h400; // Exponent for 1.0
	f.sfd = 52'h12456789ABCDE; // Random mantissa

	// Floating-point number g
	g.sign = False;
	g.exp = 11'h3FE; // Exponent for 0.5
	g.sfd = 52'h976543210123; // Random mantissa


      pipe.send(tuple2(f,g));
      $display("cycle:",rg_cycle," ",fshow(f),fshow(g));

      //`logLevel( tb, 1, $format("TB sqrt: cycle %d: send operands ", rg_cycle, fshow(f)))
    endrule


    rule rl_send4(rg_cycle==4); //sending 5th set of inputs in 5th cycle 
      FloatingPoint#(11, 52) f,g;


	// Floating-point number f
	f.sign = False; // 0
	f.exp = 11'h400; // Exponent for 1.0
	f.sfd = 52'h123456789ABC1; // Random mantissa

	// Floating-point number g
	g.sign = False; // 0
	g.exp = 11'h3FE; // Exponent for 0.5
	g.sfd = 52'h9876543210102; // Random mantissa


      
      pipe.send(tuple2(f,g));
      $display("cycle:",rg_cycle," ",fshow(f),fshow(g));

      //`logLevel( tb, 1, $format("TB sqrt: cycle %d: send operands ", rg_cycle, fshow(f)))
    endrule



    rule rl_receive;
      //Tuple3#(Bit#(1),Bit#(11), Bit#(106)) r;
      $display("----output------");
     let r <- pipe.receive();
      
      $display("cycle:",rg_cycle," ", fshow(r));
      
    endrule

    rule rl_end;
      rg_cycle <= rg_cycle + 1;  //incrementing the clock
      if (rg_cycle > 10) begin
		$finish(0);
      end
    endrule
endmodule : mkTest
///////////////////end of testbench////////////////////////////


///////////////////functions///////////////////////////////////
function Bit#(106) fn_gen_pp_dp(Bit#(53) a, Bit#(6) b);                    //generates the partial products for mantissa multiplication
	 Bit#(60) ans=0;
	 Bit#(60) op1=zeroExtend(a);
	 Bit#(60) res1=0;
	 Bit#(60) res2=0;
	 Bit#(60) res3=0;
	 Bit#(60) res4=0;
	 Bit#(60) res5=0;
     Bit#(60) res6=0;
     
	 res1 =  (b[0]==1'b1)?(op1):60'd0;
	 res2 =  (b[1]==1'b1)?(op1<<1):60'd0;
	 res3 =  (b[2]==1'b1)?(op1<<2):60'd0;
	 res4 =  (b[3]==1'b1)?(op1<<3):60'd0;
	 res5 =  (b[4]==1'b1)?(op1<<4):60'd0;
	 res6 =  (b[5]==1'b1)?(op1<<5):60'd0;

	 ans =  res1+res2+res3+res4+res5+res6;
     
	 return zeroExtend(ans);          // extending the result
endfunction
//////////////////end of functions/////////////////////////////

/////////////module/////////////////////
(*synthesize*)
module mkPDpMul(Ifc_fpu_multiplier_dp);
//declarations
Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_copy1 <- mkReg(tuple2(unpack(0),unpack(0)));
   //validity regs functioning as pipelines
   FIFO#(Bool) valid_send   <- mkLFIFO();
   FIFO#(Bool) valid_stage1 <- mkLFIFO();
   FIFO#(Bool) valid_stage2 <- mkLFIFO();
   FIFO#(Bool) valid_stage3 <- mkLFIFO();
   FIFO#(Bool) valid_stage4 <- mkLFIFO();
   FIFO#(Bool) valid_receive <- mkLFIFO();
 

   //sign regs
    Reg#(Bool) r_sgn_1 <- mkReg(False);
    Reg#(Bool) r_sgn_2 <- mkReg(False);
    Reg#(Bool) r_sgn_3 <- mkReg(False);
    Reg#(Bool) r_sgn_4 <- mkReg(False);
    Reg#(Bool) r_sgn_fin <- mkReg(False);
  //exponent regs
    Reg#(Bit#(11)) rg_exp1_1 <- mkReg(0);
    Reg#(Bit#(11)) rg_exp2_1 <- mkReg(0);
    Reg#(Bit#(12)) rg_sum_exp <- mkReg(0);
    Reg#(Bit#(12)) rg_unnorm_exp0 <- mkReg(0);
    Reg#(Bit#(12)) rg_unnorm_exp1 <- mkReg(0);
    //Reg#(Bit#(12)) rg_unnorm_exp2 <- mkReg(0);
    Reg#(Bit#(12)) rg_fin_exp <- mkReg(0);
  //mantissa product regs
    Reg#(Bit#(106)) rg_prod_par0_1 <- mkReg(0); 
	Reg#(Bit#(106)) rg_prod_par1_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par2_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par3_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par4_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par5_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par6_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par7_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par8_1 <- mkReg(0);

    Reg#(Bit#(106)) rg_prod_par0_2 <- mkReg(0); 
	Reg#(Bit#(106)) rg_prod_par1_2 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par2_2 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par3_2 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par4_2 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par5_2 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par6_2 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par7_2 <- mkReg(0);
	Reg#(Bit#(106)) rg_prod_par8_2 <- mkReg(0);
    
    Reg#(Bit#(106)) rg_prod_par012 <- mkReg(0);
    Reg#(Bit#(106)) rg_prod_par345 <- mkReg(0);
    Reg#(Bit#(106)) rg_prod_par678 <- mkReg(0);
    
    Reg#(Bit#(106)) rg_unnorm_mp <- mkReg(0);
    
    Reg#(Bit#(52)) rg_fin_m <- mkReg(0);

//rules/stages
    
    rule rl_stage1 (valid_send.first());//get the partial products from fn_gen_pp_dp and check validity
    Bit#(53) op_1m =0;
    Bit#(53) op_2m =0;
    match {.op1, .op2} =d_copy1;
    op_1m= (zeroExtend(op1.sfd) | 53'h10000000000000);
    op_2m= (zeroExtend(op2.sfd) | 53'h10000000000000);
   
    r_sgn_1 <= (op1.sign!=op2.sign);//evaluating the sign
    //incomplete--checking if the exponents are valid or not and passing them
    rg_exp1_1 <= op1.exp;
    rg_exp2_1 <= op2.exp;
    //evaluating partial products for the mantissa multiplication
    rg_prod_par0_1 <= fn_gen_pp_dp(op_1m,op_2m[5:0]); 
	rg_prod_par1_1 <= fn_gen_pp_dp(op_1m,op_2m[11:6]);
	rg_prod_par2_1 <= fn_gen_pp_dp(op_1m,op_2m[17:12]);
	rg_prod_par3_1 <= fn_gen_pp_dp(op_1m,op_2m[23:18]);
	rg_prod_par4_1 <= fn_gen_pp_dp(op_1m,op_2m[29:24]);
	rg_prod_par5_1 <= fn_gen_pp_dp(op_1m,op_2m[35:30]);
	rg_prod_par6_1 <= fn_gen_pp_dp(op_1m,op_2m[41:36]);
	rg_prod_par7_1 <= fn_gen_pp_dp(op_1m,op_2m[47:42]);
	rg_prod_par8_1 <= fn_gen_pp_dp(op_1m,zeroExtend(op_2m[52:48]));
    
    valid_stage1.enq(True);
    valid_send.deq();
    endrule


    rule rl_stage2 (valid_stage1.first()) ;//shift the partial products by 6, 12, ..48 bits for the respective products
    r_sgn_2 <= r_sgn_1;
    //adding the exponents
    rg_sum_exp <= zeroExtend(rg_exp1_1) + zeroExtend(rg_exp2_1);
    rg_prod_par0_2 <= rg_prod_par0_1;
    rg_prod_par1_2 <= rg_prod_par1_1<<6;
    rg_prod_par2_2 <= rg_prod_par2_1<<12;
    rg_prod_par3_2 <= rg_prod_par3_1<<18;
    rg_prod_par4_2 <= rg_prod_par4_1<<24;
    rg_prod_par5_2 <= rg_prod_par5_1<<30;
    rg_prod_par6_2 <= rg_prod_par6_1<<36;
    rg_prod_par7_2 <= rg_prod_par7_1<<42;
    rg_prod_par8_2 <= rg_prod_par8_1<<48;
   
    valid_stage2.enq(True);
    valid_stage1.deq();
    endrule
    
    rule rl_stage3(valid_stage2.first());
    r_sgn_3 <= r_sgn_2;
    //subtracting the bias
    rg_unnorm_exp0 <= rg_sum_exp +negate(12'h3FF);

    rg_prod_par012 <= rg_prod_par0_2 + rg_prod_par1_2+ rg_prod_par2_2;
    rg_prod_par345 <= rg_prod_par3_2 + rg_prod_par4_2+ rg_prod_par5_2;
    rg_prod_par678 <= rg_prod_par6_2 + rg_prod_par7_2+ rg_prod_par8_2;
    //$display("checking=%x", rg_prod_par8_2);
    valid_stage3.enq(True);
    valid_stage2.deq();
    endrule

    rule rl_stage4(valid_stage3.first());
    r_sgn_4 <= r_sgn_3;
    rg_unnorm_exp1 <= rg_unnorm_exp0;
    rg_unnorm_mp <= rg_prod_par012 + rg_prod_par345 + rg_prod_par678;
   // $display("in_stage3 exp=", rg_unnorm_exp0);
    valid_stage4.enq(True);
    valid_stage3.deq();
    endrule
    
    rule rl_stage5(valid_stage4.first());
    r_sgn_fin <= r_sgn_4;
    rg_fin_exp <= (rg_unnorm_mp[105]==1'b1)?(rg_unnorm_exp1+1):rg_unnorm_exp1;
    rg_fin_m   <= (rg_unnorm_mp[105]==1'b1)?(rg_unnorm_mp[104:53]):rg_unnorm_mp[103:52];
    //$display("sigbits",rg_unnorm_mp[105]);
    valid_receive.enq(True);
    valid_stage4.deq();
    endrule
  

//methods
method Action send(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52)) data_in);
    d_copy1<=data_in;
    valid_send.enq(True);
    //$display("operands ",fshow(op1),fshow(op2));
    
endmethod

method ActionValue#(FloatingPoint#(11,52)) receive() if (valid_receive.first());
    
    FloatingPoint#(11,52) ans;

    ans.sign= r_sgn_fin;
    ans.exp= truncate(rg_fin_exp);
    ans.sfd= rg_fin_m ;
    valid_receive.deq();
    //$display("here");
    return ans;
endmethod

endmodule: mkPDpMul
////////////end of module////////////////
endpackage: PDpMul




































