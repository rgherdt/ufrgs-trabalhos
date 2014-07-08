library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity datapath is
port (clock       : in std_logic;
      control_in  : in ctlcod_type;
      control_out : out ctlcod_type;
      mem_in      : in bus8;
      mem_out     : out bus8;
      ri_out      : out bus8;
      alu_res     : in signed(7 downto 0);
      alu_x       : out signed(7 downto 0);
      alu_y       : out signed(7 downto 0));
end datapath;

architecture dpath of datapath is

    component reg8 is
    port (reg_in : in bus8;
          ld, clk : in std_logic;
          reg_out : out bus8);
    end component;

    component count8 is
    port (clk,ld : in std_logic;
          din : in bus8;
          count : out bus8);
    end component;

    component mux16_8 is
    port (clk, sel : in std_logic;
          mux_in0 : in bus8;
          mux_in1 : in bus8;
          mux_out : out bus8);
    end component;

    signal alu_op : aluop_type;
    signal ac_ld, pc_inc, pc_ld, mpx_sel, rem_ld, mem_rd, mem_wr, rdm_ld,
           ri_ld, nflag, zflag, cflag, vflag, bflag, flags_ld: std_logic;
    signal mem_out_bus, mem_in_bus : bus8;
    signal ac_out, rrdm_out, pc_out, mpx_out : bus8;

begin
    alu_op <= control_in(0) & control_in(1) & control_in(2);
    ac_ld    <= control_in(3);
    pc_inc   <= control_in(4);
    pc_ld    <= control_in(5);
    mpx_sel  <= control_in(6);
    rem_ld   <= control_in(7);
    mem_rd   <= control_in(8);
    mem_wr   <= control_in(9);
    rdm_ld   <= control_in(10);
    ri_ld    <= control_in(11);
    nflag    <= control_in(12);
    zflag    <= control_in(13);
    cflag    <= control_in(14);
    vflag    <= control_in(15);
    bflag    <= control_in(16);
    flags_ld <= control_in(17);
    
    accum : reg8
    port map (reg_in  => std_logic_vector(alu_res),
              clk     => clock,
              ld      => ac_ld,
              reg_out => ac_out);
    
    rrdm : reg8
    port map (reg_in  => mem_out_bus,
              clk     => clock,
              ld      => rdm_ld,
              reg_out => rrdm_out);

    wrdm : reg8
    port map (reg_in  => ac_out,
              clk     => clock,
              ld      => rdm_ld,
              reg_out => mem_in_bus);

    pc : count8
    port map (din     => rrdm_out,
              clk     => clock,
              ld      => pc_ld,
              count   => pc_out);

    mpx : mux16_8
    port map (clk     => clock,
              sel     => mpx_sel,
              mux_in0 => pc_out,
              mux_in1 => rrdm_out,
              mux_out => mpx_out);

    ri : reg8
    port map (reg_in  => rrdm_out,
              clk     => clock,
              ld      => ri_ld,
              reg_out => ri_out);

    mem_out    <= mem_out_bus;
    mem_in_bus <= mem_in;

end dpath;

    
    
