library ieee;
use ieee.std_logic_1164.all;

entity ahmes_datapath is
port (clk       : in std_logic;
      control_in  : in std_logic_vector(20 downto 0);
      flags_in    : in std_logic_vector(4 downto 0);
      flags_out   : out std_logic_vector(4 downto 0);
      dec_out     : out std_logic_vector(23 downto 0);
      mem_in      : in std_logic_vector(7 downto 0);
      mem_out     : out std_logic_vector(7 downto 0);
      alu_res     : in std_logic_vector(7 downto 0);
      alu_x       : out std_logic_vector(7 downto 0);
      alu_y       : out std_logic_vector(7 downto 0);
      dout        : out std_logic_vector(7 downto 0));
end ahmes_datapath;

architecture dpath of ahmes_datapath is
    constant NOPCOD : std_logic_vector(7 downto 0) := x"00";
    constant STACOD : std_logic_vector(7 downto 0) := x"10";
    constant LDACOD : std_logic_vector(7 downto 0) := x"20";
    constant ADDCOD : std_logic_vector(7 downto 0) := x"30";
    constant ORCOD  : std_logic_vector(7 downto 0) := x"40";
    constant ANDCOD : std_logic_vector(7 downto 0) := x"50";
    constant NOTCOD : std_logic_vector(7 downto 0) := x"60";
    constant SUBCOD : std_logic_vector(7 downto 0) := x"70";
    constant JMPCOD : std_logic_vector(7 downto 0) := x"80";
    constant JNCOD  : std_logic_vector(7 downto 0) := x"90";
    constant JPCOD  : std_logic_vector(7 downto 0) := x"94";
    constant JVCOD  : std_logic_vector(7 downto 0) := x"98";
    constant JNVCOD : std_logic_vector(7 downto 0) := x"9c";
    constant JZCOD  : std_logic_vector(7 downto 0) := x"a0";
    constant JNZCOD : std_logic_vector(7 downto 0) := x"a4";
    constant JCCOD  : std_logic_vector(7 downto 0) := x"b0";
    constant JNCCOD : std_logic_vector(7 downto 0) := x"b4";
    constant JBCOD  : std_logic_vector(7 downto 0) := x"b8";
    constant JNBCOD : std_logic_vector(7 downto 0) := x"bc";
    constant SHRCOD : std_logic_vector(7 downto 0) := x"e0";
    constant SHLCOD : std_logic_vector(7 downto 0) := x"e1";
    constant RORCOD : std_logic_vector(7 downto 0) := x"e2";
    constant ROLCOD : std_logic_vector(7 downto 0) := x"e3";
    constant HLTCOD : std_logic_vector(7 downto 0) := x"f0";

    component pc is
    port (din : in std_logic_vector(7 downto 0);
          inc, ld, clk : in std_logic;
          dout : out std_logic_vector(7 downto 0));
    end component;

    component ahmes_shifter is
    port (clk, ld, shl_flag, shr_flag, rol_flag, ror_flag, cflag_in : std_logic;
          din : in std_logic_vector(7 downto 0);
          dout : out std_logic_vector(7 downto 0);
          nflag, zflag, cflag: out std_logic);
    end component;

    component reg8 is
    port (reg_in : in std_logic_vector(7 downto 0);
          ld, clk : in std_logic;
          reg_out : out std_logic_vector(7 downto 0));
    end component;

    component flags_reg is
    port (ld, clk   : in std_logic;
          nflag_in  : in std_logic;
          zflag_in  : in std_logic;
          cflag_in  : in std_logic;
          vflag_in  : in std_logic;
          bflag_in  : in std_logic;
          nflag_out : out std_logic;
          zflag_out : out std_logic;
          cflag_out : out std_logic;
          vflag_out : out std_logic;
          bflag_out : out std_logic);
    end component;

    component counter8 is
    port (clk,ld : in std_logic;
          din : in std_logic_vector(7 downto 0);
          count : out std_logic_vector(7 downto 0));
    end component;

    component mux16_8 is
    port (clk, sel : in std_logic;
          mux_in0 : in std_logic_vector(7 downto 0);
          mux_in1 : in std_logic_vector(7 downto 0);
          mux_out : out std_logic_vector(7 downto 0));
    end component;

    signal alu_add, alu_or, alu_and, alu_not, alu_sub, alu_passy,
           ctl_shr, ctl_shl, ctl_ror, ctl_rol : std_logic;
    signal ac_ld, pc_inc, pc_ld, mpx_sel, rem_ld, mem_rd, mem_wr, rrdm_ld,
           wrdm_ld, ri_ld, flags_ld: std_logic;
    signal mem_out_bus, mem_in_bus : std_logic_vector(7 downto 0);
    signal ac_out, rrdm_out, pc_out, mpx_out : std_logic_vector(7 downto 0);
    signal ri_out : std_logic_vector(7 downto 0);
    signal nflag_in, zflag_in, cflag_in, vflag_in, bflag_in,
           nflag, zflag, cflag, vflag, b_flag,   
           nflag_out, zflag_out, cflag_out, vflag_out, bflag_out : std_logic;

begin
    nflag_in <= flags_in(4);
    zflag_in <= flags_in(3);
    cflag_in <= flags_in(2);
    vflag_in <= flags_in(1);
    bflag_in <= flags_in(0);

    alu_add   <= control_in(20);
    alu_or    <= control_in(19);
    alu_and   <= control_in(18);
    alu_not   <= control_in(17);
    alu_sub   <= control_in(16);
    alu_passy <= control_in(15);
    ctl_shr   <= control_in(14);
    ctl_shl   <= control_in(13);
    ctl_ror   <= control_in(12);
    ctl_rol   <= control_in(11);
    ac_ld     <= control_in(10);
    flags_ld  <= control_in(9);
    pc_inc    <= control_in(8);
    pc_ld     <= control_in(7);
    mpx_sel   <= control_in(6);
    rem_ld    <= control_in(5);
    mem_rd    <= control_in(4);
    mem_wr    <= control_in(3);
    rrdm_ld   <= control_in(2);
    wrdm_ld   <= control_in(1);
    ri_ld     <= control_in(0);

    ri : reg8
    port map (reg_in  => rrdm_out,
              clk     => clk,
              ld      => ri_ld,
              std_logic_vector(reg_out) => ri_out);

    accum : ahmes_shifter
    port map (din      => alu_res,
              clk      => clk,
              ld       => ac_ld,
              shl_flag => ctl_shl,
              shr_flag => ctl_shr,
              rol_flag => ctl_rol,
              ror_flag => ctl_ror,
              dout     => ac_out,
              cflag_in => cflag_in,
              cflag    => cflag,
              nflag    => nflag,
              zflag    => zflag);


    flags : flags_reg
    port map (ld        => flags_ld,
              clk       => clk,
              nflag_in  => nflag,
              zflag_in  => zflag,
              cflag_in  => cflag,
              vflag_in  => vflag_in,
              bflag_in  => bflag_in, 
              nflag_out => nflag_out, 
              zflag_out => zflag_out,
              cflag_out => cflag_out,
              vflag_out => vflag_out,
              bflag_out => bflag_out);

    decode : process (clk)
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

    
    rrdm : reg8
    port map (reg_in  => mem_out_bus,
              clk     => clk,
              ld      => rrdm_ld,
              reg_out => rrdm_out);

    wrdm : reg8
    port map (reg_in  => ac_out,
              clk     => clk,
              ld      => wrdm_ld,
              std_logic_vector(reg_out) => mem_in_bus);

    program_counter : pc
    port map (din     => rrdm_out,
              inc     => pc_inc,
              clk     => clk,
              ld      => pc_ld,
              dout => pc_out);

    mpx : mux16_8
    port map (clk     => clk,
              sel     => mpx_sel,
              mux_in0 => pc_out,
              mux_in1 => rrdm_out,
              mux_out => mpx_out);

    mem_out    <= mem_out_bus;
    mem_in_bus <= mem_in;
    dout  <= ac_out;

end dpath;
    
