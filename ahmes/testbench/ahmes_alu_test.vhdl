library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity test_alu is
end test_alu;

architecture test_alu_behv of test_alu is
    component alu is
    port (x : in bus8;
          y : in bus8;
          sel : in std_logic_vector(1 downto 0);
          alu_out : out bus8;
          st_reg : out std_logic_vector(4 downto 0));
    end component;

    signal sel : std_logic_vector(1 downto 0);
    signal x, y, alu_out : bus8;
begin
    x <= "00001000", "00000001" after 15 ns;
    y <= "00000100", "00000010" after 25 ns;
    sel <= "00";
    r: alu port map (x, y, sel, alu_out);
end test_alu_behv;
