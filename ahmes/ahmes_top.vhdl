library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity ahmes is
port (clock: in std_logic;
      reset: in std_logic);
end ahmes;

architecture ahmes of ahmes is
    type state_type is (init, fetch0, fetch1, fetch2, decod, 

    component alu is
    port (x : in bus8;
          y : in bus8;
          alu_sel : in std_logic_vector(1 downto 0);
          alu_out : out bus8;
          n_alu, z_alu, c_alu, v_alu, b_alu : out std_logic;
    end component;

    component datapath is
    port (clock       : in std_logic;
          control_in  : in ctlcod_type;
          control_out : out ctlcod_type;
          mem_in      : in bus8;
          mem_out     : out bus8;
          alu_res     : in bus8;
          alu_x       : out bus8;
          alu_y       : out bus8;
    end component;

    signal mem : memory_type;
    signal control_in  : in ctlcod_type;
    signal flags_out   : out std_logic_vector(4 downto 0);
    signal alu_op : alusel_type;
    signal ac_ld, pc_inc, pc_ld, mpx_sel, rem_ld, mem_rd, mem_wr, rdm_ld,
           ri_ld, nflag, zflag, cflag, vflag, bflag, flags_ld : std_logic;

    alu_op   <= control_in(0) & control_in(1) & control_in(2);
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

begin    
    ahmes_alu : alu
    port map (x     => 

   
begin

