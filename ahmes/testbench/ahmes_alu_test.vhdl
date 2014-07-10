library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity test_alu is
end test_alu;

architecture test_alu_behv of test_alu is

    component alu is
    port (x : in signed(7 downto 0);
          y : in signed(7 downto 0);
          alu_opsel : in std_logic_vector(5 downto 0);
          alu_out : out signed(7 downto 0);
          flags_out : out std_logic_vector(4 downto 0));
    end component;

    signal sel : std_logic_vector(5 downto 0);
    signal flags : std_logic_vector(4 downto 0);
    signal x, y, alu_out : signed(7 downto 0);
begin
    flags <= "00000";
    x <= "00001000", "00000001" after 15 ns;
    y <= "00000100", "00000010" after 25 ns;
    sel <= "100000";
    r: alu port map (
            x => x,
            y => y,
            alu_opsel => sel,
            alu_out   => alu_out,
            flags_out => flags);
end test_alu_behv;
