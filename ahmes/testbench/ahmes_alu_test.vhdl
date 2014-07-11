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
    port (x : in signed(7 downto 0);
          y : in signed(7 downto 0);
          alu_opsel : in std_logic_vector(9 downto 0);
          alu_out : out signed(7 downto 0);
          flags_out : out std_logic_vector(4 downto 0));
    end component;

    signal clock : std_logic;
    signal sel : std_logic_vector(9 downto 0);
    signal flags : std_logic_vector(4 downto 0);
    signal flags_ld : std_logic;
    signal flags_out : std_logic_vector(4 downto 0);
    signal x, y, alu_out : signed(7 downto 0);

begin
    flags_ld <= '0';
    flagsReg : reg5
    port map (reg_in  => flags,
              clk     => clock,
              ld      => flags_ld,
              reg_out => flags_out);

    clock <= '0', '1' after 1 ns, '0' after 2 ns, '1' after 3 ns, '0' after 4 ns, '1' after 5 ns,
             '0' after 6 ns, '1' after 7 ns, '0' after 8 ns, '1' after 9 ns, '0' after 10 ns, '1' after 11 ns;
    flags <= "00000";
    x <= "00000010";
    y <= "00000011";
    sel <= "0000010000", "1000000000" after 1 ns, "1000000000" after 4 ns;
    r: alu port map (
--            clk => clock,
            x => x,
            y => y,
            alu_opsel => sel,
            alu_out   => alu_out,
            flags_out => flags);
end test_alu_behv;
