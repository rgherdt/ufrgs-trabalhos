library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity test_alu is
end test_alu;

architecture test_alu_behv of test_alu is

    component alu is
    port (x : in signed(0 to 7);
          y : in signed(0 to 7);
          alu_opsel : in std_logic_vector(0 to 5);
          alu_out : out signed(0 to 7);
          flags_out : out std_logic_vector(0 to 4));
    end component;

    signal sel : std_logic_vector(0 to 5);
    signal flags : std_logic_vector(0 to 4);
    signal x, y, alu_out : signed(0 to 7);
begin
    flags <= "00000";
    alu_out <= "00000000";
    x <= "00001000", "00000001" after 5 ns;
    y <= "00000100", "00000010" after 10 ns;
    sel <= "100000";
    r: alu port map (
            x => x,
            y => y,
            alu_opsel => sel,
            alu_out   => alu_out,
            flags_out => flags);
end test_alu_behv;
