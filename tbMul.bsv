package TbMul_pkg;
//imported libraries
import FloatingPoint ::*;
import FIFO::*;

interface Ifc_mul_dp;                                              
	method Action send(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52)) data_in);
	method Tuple3#(Bit#(1),Bit#(13), Bit#(106)) receive();
endinterface



(* synthesize *)

module mkTbMul (Empty);

	Reg#(int) x <- mkReg ('h10);
	Reg#(int) y1 <- mkReg ('h00);
	Ifc_mul_dp pipe <- mkMulPipe;
	
    Reg#(Bit#(8)) rg_cycle <- mkReg(0);

//Testcases

    rule rl_send0(rg_cycle==0); //sending first set of inputs in first cycle 
      FloatingPoint#(11, 52) f,g;

      f.sign = True;
      f.exp = 11'h401;
      f.sfd = 52'h4000000000000;
      
      g.sign = False;
      g.exp = 11'h400;
      g.sfd = 52'h0;


      pipe.send(tuple2(f,g));
      $display("cycle:",rg_cycle," ",fshow(f),fshow(g));

      //`logLevel( tb, 1, $format("TB sqrt: cycle %d: send operands ", rg_cycle, fshow(f)))
    endrule
    
    rule rl_send2(rg_cycle==2);   //sending first set of inputs in third cycle
      FloatingPoint#(11, 52) f,g;

      f.sign = True;
      f.exp = 11'h01;
      f.sfd = 52'h000001;
      
      g.sign = False;
      g.exp = 11'h7e;
      g.sfd = 52'h7ffffd;


      pipe.send(tuple2(f,g));
      $display("cycle:",rg_cycle," ",fshow(f),fshow(g));

    endrule
     
    rule rl_send3(rg_cycle==3); //sending first set of inputs in fourth cycle
      FloatingPoint#(11, 52) f,g;

      f.sign = True;
      f.exp = 11'h01;
      f.sfd = 52'h000001;
      
      g.sign = False;
      g.exp = 11'h7e;
      g.sfd = 52'h7ffffd;


      pipe.send(tuple2(f,g));
      $display("cycle:",rg_cycle," ",fshow(f),fshow(g));

      //`logLevel( tb, 1, $format("TB sqrt: cycle %d: send operands ", rg_cycle, fshow(f)))
    endrule

    rule rl_receive;
      //Tuple3#(Bit#(1),Bit#(11), Bit#(106)) r;

      let r = pipe.receive();
      
      $display("cycle:",rg_cycle," sign: %h, ",tpl_1(r), "exp: %h, ",tpl_2(r)[10:0], "man: %h, ",tpl_3(r)[105:55]);
      
    endrule

    rule rl_end;
      rg_cycle <= rg_cycle + 1;  //incrementing the clock
      if (rg_cycle > 7) begin
		$finish(0);
      end
    endrule
endmodule: mkTbMul

function Bit#(2) ha(Bit#(1) a, Bit#(1) b);
	return {a&b, a^b};
endfunction	

function Tuple2#(Bit#(1), Bit#(1)) fa(Bit#(1) a, Bit#(1) b, Bit#(1) cin);
	Bit#(2) t = ha(a,b);
	Bit#(1) cout;
	let t2 = ha(t[0], cin);
	cout = t2[1]|t[1];
	return tuple2(cout, t2[0]);
endfunction

function int incr1(int a);
	return a+1;
endfunction


function Tuple2#(Bit#(32), Bit#(32)) partialProd(Bit#(32) a, Bit#(1) bi, Bit#(32) sum, Bit#(32) carry);
	Bit#(32) sumout;
	Bit#(32) cout;
	for (Integer i=0; i<32; i=i+1) begin
		let aa = fa(a[i]&bi, sum[i], carry[i]);
		cout[i] = tpl_1(aa);
		sumout[i] = tpl_2(aa);
	end
	return tuple2(cout,sumout);		
endfunction

function Bit#(13) eval_exp(Bit#(11) e1, Bit#(11) e2); 

	 Bit#(13) er= zeroExtend(e1)+zeroExtend(e2)+negate(13'd1023); 
	 return er;
	 
endfunction

function Bit#(106) fn_gen_pp_dp(Bit#(53) m, Bit#(6) b);                    //generates the partial products for mantissa multiplication
	 Bit#(60) y=0;
	 Bit#(60) a=zeroExtend(m);
	 Bit#(60) res1=0;
	 Bit#(60) res2=0;
	 Bit#(60) res3=0;
	 Bit#(60) res4=0;
	 Bit#(60) res6=0;
	 Bit#(60) res5=0;
	 res1 = (b[0]==1'b1)?(a):60'd0;
	 res2 =  (b[1]==1'b1)?(a<<1):60'd0;
	 res3 =  (b[2]==1'b1)?(a<<2):60'd0;
	 res4 =  (b[3]==1'b1)?(a<<3):60'd0;
	 res5 =  (b[4]==1'b1)?(a<<4):60'd0;
	 res6 =  (b[5]==1'b1)?(a<<5):60'd0;
	 y =  res1+res2+res3+res4+res5+res6;
	 return zeroExtend(y);          // extending the result
endfunction

function Bit#(106) fn_add_dp4(Bit#(106) x1,Bit#(106) x2,Bit#(106) x3,Bit#(106) x4);
	 let x5 = x1+x2+x3+x4;             //generating product from partial products
	 return x5;
endfunction

function Bit#(106) fn_add_dp3(Bit#(106) x1,Bit#(106) x2,Bit#(106) x3);
	 let x5 = x1+x2+x3;             //generating product from partial products
	 return x5;
endfunction


(* synthesize *)

module mkMulPipe (Ifc_mul_dp);
	
	Reg#(Bit#(13)) rg_er<-mkReg(0);
	Reg#(Bit#(1)) rg_sr<-mkReg(0);
	
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_1 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_2 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_3 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_4 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_5 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_6 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_7 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_8 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_9 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_10 <- mkReg(tuple2(unpack(0),unpack(0)));
	Reg#(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52))) d_11 <- mkReg(tuple2(unpack(0),unpack(0)));
	
	rule rl_eval_exponent_sign_stage1;                                         
	  match {.opA, .opB} = d_1;
	  rg_er <= eval_exp(opA.exp,opB.exp);                   //evaluating exponent, 1st stage
	  rg_sr <= pack(opA.sign != opB.sign);                                         //evaluating sign, 1st stage
	endrule
	
	Reg#(Bit#(106)) rg_partial_product0_1 <- mkReg(0); // size 60 or 59
	Reg#(Bit#(106)) rg_partial_product1_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_partial_product2_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_partial_product3_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_partial_product4_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_partial_product5_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_partial_product6_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_partial_product7_1 <- mkReg(0);
	Reg#(Bit#(106)) rg_partial_product8_1 <- mkReg(0);
	
	
	rule rl_eval_partial_product_1_stage1;                  
	  match {.opA, .opB} = d_2;                                  
	  Bit#(11) expA1 = 0;
	  Bit#(11) expB1 = 0;
	  Bit#(52) sfdA1 = 0;
	  Bit#(52) sfdB1 = 0;
	  
	  sfdA1 = opA.sfd;
	  expA1 = opA.exp;
	  sfdB1 = opB.sfd;
	  expB1 = opB.exp;
	  
	  rg_partial_product0_1<=fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[5:0]);
	  rg_partial_product1_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[11:6])[99:0],6'd0};	  
	  rg_partial_product2_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[17:12])[93:0],12'd0};	  
	  rg_partial_product3_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[23:18])[87:0],18'd0};	  
	  rg_partial_product4_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[29:24])[81:0],24'd0};	  
	  rg_partial_product5_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[35:30])[75:0],30'd0};	  
	  rg_partial_product6_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[41:36])[69:0],36'd0};	  
	  rg_partial_product7_1<={fn_gen_pp_dp({(|expA1),sfdA1},sfdB1[47:42])[63:0],42'd0};
	  rg_partial_product8_1<={fn_gen_pp_dp({(|expA1),sfdA1},{1'b0,(|expB1),sfdB1[51:48]})[57:0],48'd0};
	  
	endrule 
	
	Reg#(Bit#(106)) rg_smr0 <- mkReg(0);
	Reg#(Bit#(106)) rg_smr1 <- mkReg(0);
	Reg#(Bit#(106)) rg_smr <- mkReg(0);
	Reg#(Bit#(13)) rg_ers2<-mkReg(0);
	Reg#(Bit#(1)) rg_srs2<-mkReg(0);
	rule rl_add_stage2;                           
	  rg_smr0<= fn_add_dp4(pack(rg_partial_product0_1),pack(rg_partial_product1_1),pack(rg_partial_product2_1),pack(rg_partial_product3_1));
	  rg_smr1<= fn_add_dp4(pack(rg_partial_product4_1),pack(rg_partial_product5_1),pack(rg_partial_product6_1),pack(rg_partial_product7_1));
	  rg_smr <= fn_add_dp3(pack(rg_smr0), pack(rg_smr1), pack(rg_partial_product8_1));
	  rg_ers2 <= rg_er;
	  rg_srs2 <= rg_sr;	  
	endrule
	
	
	Reg#(Bit#(1)) rg_sign <- mkReg(0);
	Reg#(Bit#(13)) rg_ex <- mkReg(0);
	Reg#(Bit#(106)) rg_man <- mkReg(0);
	rule rl_stage3;
	
		rg_sign <= rg_srs2;
		rg_ex <= rg_ers2;
		rg_man <= rg_smr;
	endrule

	
	

	// send and receive methods
	method Action send(Tuple2#(FloatingPoint#(11,52),FloatingPoint#(11,52)) data_in);
	 d_1<=data_in;
	 d_2<=data_in;
	 d_3<=data_in;
	 d_4<=data_in;
	 d_5<=data_in;
	 d_6<=data_in;
	 d_7<=data_in;
	 d_8<=data_in;
	 d_9<=data_in;
	 d_10<=data_in;
	 d_11<=data_in;
	endmethod


/*
method ReturnType#(8,23) receive();                                      
  return case (pack(rg_sp_ex3))
    3'b000:  ReturnType{valid: pack(rg_sp_valid[4]),value:tpl_1(rg_out),ex:tpl_2(rg_out)};
    3'b100:  ReturnType{valid: pack(rg_sp_valid[4]),value:FloatingPoint{sign: unpack(1'b0),exp: 8'hFF,sfd: truncate(24'h400000)},ex:unpack(5'b10000)};
    3'b010:  ReturnType{valid: pack(rg_sp_valid[4]),value:FloatingPoint{sign: tpl_1(rg_out).sign,exp: 8'hFF,sfd: 23'd0},ex:defaultValue};
    3'b001:  ReturnType{valid: pack(rg_sp_valid[4]),value:FloatingPoint{sign: tpl_1(rg_out).sign,exp: 8'd0,sfd: 23'd0},ex:defaultValue};
    3'b111:  ReturnType{valid: pack(rg_sp_valid[4]),value:FloatingPoint{sign: unpack(1'b0),exp: 8'hFF,sfd: truncate(24'h400000)},ex:unpack(5'b00000)};
   endcase;
endmethod
*/
method Tuple3#(Bit#(1),Bit#(13), Bit#(106)) receive();                                      
	return tuple3(rg_sign, rg_ex, rg_man);
endmethod



endmodule: mkMulPipe
endpackage: TbMul_pkg
