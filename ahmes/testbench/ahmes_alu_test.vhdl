library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
use work.ahmes_lib.all;

entity test_alu is
end test_alu;

architecture test_alu_behv of test_alu is
    component alu is
    port (a : in std_logic_vector(7 downto 0);
          b : in std_logic_vector(7 downto 0);
          sel : in std_logic_vector(1 downto 0);
          alu_out : out std_logic_vector(7 downto 0);
          st_reg : out std_logic_vector(4 downto 0));
    end component;

    signal sel : std_logic_vector(1 downto 0);
    signal a, b, alu_out : std_logic_vector(7 downto 0);
begin
    a <= "00001000", "00000001" after 15 ns;
    b <= "00000100", "00000010" after 25 ns;
    sel <= "00";
    r: alu port map (a, b, sel, alu_out);
end test_alu_behv;
