library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;
use ieee.std_logic_arith.all;
use work.ahmes_lib.all;

entity datapath is
port (clock  : in std_logic;
      alusel : in std_logic;
      control_in : in std_logic_vector(15 downto 0);
      --ac_ld, pc_ld, ld_rem, rdm_ld, ri_ld, ld_nz : in std_logic);
end datapath;

architecture dpath of datapath is

    component alu is
    port (a : in std_logic_vector(7 downto 0);
          b : in std_logic_vector(7 downto 0);
          sel : in std_logic_vector(1 downto 0);
          alu_out : out std_logic_vector(7 downto 0);
          st_reg : out std_logic_vector(4 downto 0);
    end component;

    component reg8 is
    port (reg_in : in std_logic_vector(7 downto 0);
          ld, clk : in std_logic;
          reg_out : out std_logic_vector(7 downto 0));
    end component;

    component count8 is
    port (clk,ld : in std_logic;
          din : in std_logic_vector (7 downto 0);
          count : out std_logic_vector (7 downto 0));
    end component;

    signal sel_alu, ac_ld, pc_inc, pc_ld, mpx_sel, rem_ld, mem_rd, mem_wr, 
           rdm_ld, ri_ld, nflag, zflag, cflag, vflag, bflag : std_logic;
    signal ac_out : std_logic_vector(7 downto 0);
    signal alu_res : std_logic_vector(7 downto 0);

begin
    sel_alu <= control_in(0);
    ac_ld   <= control_in(1);
    pc_inc  <= control_in(2);
    pc_ld   <= control_in(3);
    mpx_sel <= control_in(4);
    rem_ld  <= control_in(5);
    mem_rd  <= control_in(6);
    mem_wr  <= control_in(7);
    rdm_ld  <= control_in(8);
    ri_ld   <= control_in(9);
    nflag   <= control_in(10);
    zflag   <= control_in(11);
    cflag   <= control_in(12);
    vflag   <= control_in(13);
    bflag   <= control_in(14);
    
    accum : reg8
    port map (reg_in  => alu_res;
              clk     => clock;
              ld      => ac_ld;
              reg_out => ac_out);
    
    rrdm : reg8
    port map (reg_in  => mem_out;
              clk     => clock;
              ld      => rdm_ld;
              reg_out => rrdm_out);

    wrdm : reg8
    port map (reg_in  => ac_out;
              clk     => clock;
              ld      => rdm_ld;
              reg_out => mem_in);

    pc : count8
    port map (din     => rrdm_out;
              clk     => clock;
              ld      => pc_ld;
              count   => pc_out);

    mpx : mux16_8
    port map (clk     => clock;
              sel     => mpx_sel;
              mux_in0 => pc_out;
              mux_in1 => rrdm_out;
              mux_out => mpx_out);

    ri : reg8
    port map (reg_in  => rrdm_out;
              clk     => clock;
              ld      => ri_ld;
              reg_out => ri_out);

end dpath;

    
    
