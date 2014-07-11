library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity test_alu is
end test_alu;

architecture test_alu_behv of test_alu is

    component reg5 is
    port (reg_in : in std_logic_vector(4 downto 0);
          ld, clk : in std_logic;
          reg_out : out std_logic_vector(4 downto 0));
    end component;

    component alu is
    port (x : in unsign8;
          y : in unsign8;
          alu_opsel : in std_logic_vector(5 downto 0);
          alu_out : out unsign8;
          flags_out : out std_logic_vector(4 downto 0));
    end component;

    component shifter is
    port (clk, ld, shl, shr, rol_flag, ror_flag, cflag_in : std_logic;
          din : in unsign8;
          dout : out unsign8;
          nflag, zflag, cflag: out std_logic);
    end component;

    signal clk : std_logic;
    signal sel : std_logic_vector(9 downto 0);
    signal flags : std_logic_vector(4 downto 0) := "00000";
    signal flags_ld, ac_ld : std_logic;
    signal flags_out : std_logic_vector(4 downto 0) := "00000";
    signal x : unsign8 := "00000100";
    signal y : unsign8 := "00000011";
    signal alu_out : unsign8 := "00000000";
    signal shift_out : unsign8;

    constant clk_period : time := 4 ns;

begin
    flags_ld <= '0';
    ac_ld    <= '0';
    flagsReg : reg5
    port map (reg_in  => flags,
              clk     => clk,
              ld      => flags_ld,
              reg_out => flags_out);


   -- Clock process definitions( clock with 50% duty cycle is generated here.
    clk_process :process
    begin
        clk <= '0';
        wait for clk_period/2;  --for 0.5 ns signal is '0'.
        clk <= '1';
        wait for clk_period/2;  --for next 0.5 ns signal is '1'.
    end process;


    sel <= "1000000000", "0000000000" after 3 ns;
    myalu: alu port map (
            x => x,
            y => y,
            alu_opsel => sel(9 downto 4),
            alu_out   => alu_out,
            flags_out => flags);

    accum : shifter port map (
            clk       => clk,
            ld        => ac_ld,
            shl       => sel(3),
            shr       => sel(2),
            rol_flag  => sel(1),
            ror_flag  => sel(0),
            cflag_in  => flags_out(2),
            din       => alu_out,
            dout      => x,
            nflag     => flags(4),
            zflag     => flags(3),
            cflag     => flags(2));
end test_alu_behv;
