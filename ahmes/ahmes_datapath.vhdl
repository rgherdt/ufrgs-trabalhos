library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity datapath is
port (clock       : in std_logic;
      control_in  : in ctlcod_type;
      flags_in    : in std_logic_vector(4 downto 0);
      flags_out   : out std_logic_vector(4 downto 0);
      dec_out     : out instdec_type;
      mem_in      : in bus8;
      mem_out     : out bus8;
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

    component shifter is
    port (clk, ld, shl, shr, rol_flag, ror_flag, cflag_in : std_logic;
          din : in std_logic_vector(7 downto 0);
          dout : out std_logic_vector(7 downto 0);
          nflag, zflag, cflag: out std_logic);
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

    signal alu_add, alu_or, alu_and, alu_not, alu_sub, alu_passy,
           ctl_shr, ctl_shl, ctl_ror, ctl_rol : std_logic;
    signal ac_ld, pc_inc, pc_ld, mpx_sel, rem_ld, mem_rd, mem_wr, rrdm_ld,
           wrdm_ld, ri_ld, flags_ld: std_logic;
    signal mem_out_bus, mem_in_bus : bus8;
    signal ac_out, rrdm_out, pc_out, mpx_out, ri_out : bus8;
    signal flags : std_logic_vector(4 downto 0);
    signal nflag, zflag, cflag, vflag, bflag : std_logic;

begin
    flags  <= flags_in;
    nflag  <= flags(0);
    zflag  <= flags(1);
    cflag  <= flags(2);
    vflag  <= flags(3);
    bflag  <= flags(4);

    alu_add   <= control_in(0);
    alu_or    <= control_in(1);
    alu_and   <= control_in(2);
    alu_not   <= control_in(3);
    alu_sub   <= control_in(4);
    alu_passy <= control_in(5);
    ctl_shr   <= control_in(6);
    ctl_shl   <= control_in(7);
    ctl_ror   <= control_in(8);
    ctl_rol   <= control_in(9);
    ac_ld     <= control_in(10);
    flags_ld  <= control_in(11);
    pc_inc    <= control_in(12);
    pc_ld     <= control_in(13);
    mpx_sel   <= control_in(14);
    rem_ld    <= control_in(15);
    mem_rd    <= control_in(16);
    mem_wr    <= control_in(17);
    rrdm_ld   <= control_in(18);
    wrdm_ld   <= control_in(19);
    ri_ld     <= control_in(20);

    ri : reg8
    port map (reg_in  => rrdm_out,
              clk     => clock,
              ld      => ri_ld,
              reg_out => ri_out);

    decode : process (clock)
    begin
        case ri_out is
            when NOPCOD => dec_out <= "100000000000000000000000";
            when STACOD => dec_out <= "010000000000000000000000";
            when LDACOD => dec_out <= "001000000000000000000000";
            when ADDCOD => dec_out <= "000100000000000000000000";
            when ORCOD  => dec_out <= "000010000000000000000000";
            when ANDCOD => dec_out <= "000001000000000000000000";
            when NOTCOD => dec_out <= "000000100000000000000000";
            when SUBCOD => dec_out <= "000000010000000000000000";
            when JMPCOD => dec_out <= "000000001000000000000000";
            when JNCOD  => dec_out <= "000000000100000000000000";
            when JPCOD  => dec_out <= "000000000010000000000000";
            when JVCOD  => dec_out <= "000000000001000000000000";
            when JNVCOD => dec_out <= "000000000000100000000000";
            when JZCOD  => dec_out <= "000000000000010000000000";
            when JNZCOD => dec_out <= "000000000000001000000000";
            when JCCOD  => dec_out <= "000000000000000100000000";
            when JNCCOD => dec_out <= "000000000000000010000000";
            when JBCOD  => dec_out <= "000000000000000001000000";
            when JNBCOD => dec_out <= "000000000000000000100000";
            when SHRCOD => dec_out <= "000000000000000000010000";
            when SHLCOD => dec_out <= "000000000000000000001000";
            when RORCOD => dec_out <= "000000000000000000000100";
            when ROLCOD => dec_out <= "000000000000000000000010";
            when HLTCOD => dec_out <= "000000000000000000000001";
            when others => dec_out <= "000000000000000000000000";
        end case;
    end process;

    accum : shifter
    port map (din      => std_logic_vector(alu_res),
              clk      => clock,
              ld       => ac_ld,
              shl      => ctl_shl,
              shr      => ctl_shr,
              rol_flag => ctl_rol,
              ror_flag => ctl_ror,
              dout     => ac_out,
              cflag_in => cflag,
              cflag    => cflag,
              nflag    => nflag,
              zflag    => zflag);
    
    rrdm : reg8
    port map (reg_in  => mem_out_bus,
              clk     => clock,
              ld      => rrdm_ld,
              reg_out => rrdm_out);

    wrdm : reg8
    port map (reg_in  => ac_out,
              clk     => clock,
              ld      => wrdm_ld,
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

    mem_out    <= mem_out_bus;
    mem_in_bus <= mem_in;
    flags_out  <= flags;

end dpath;

    
    
