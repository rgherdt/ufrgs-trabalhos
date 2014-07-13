library ieee;
use ieee.std_logic_1164.all;

entity reg8 is
port (reg_in : in std_logic_vector(7 downto 0);
      ld, clk : in std_logic;
      reg_out : out std_logic_vector(7 downto 0));
end reg8;

architecture reg8 of reg8 is
begin
    reg: process(clk)
    begin
        if (rising_edge(clk)) then
            if (ld = '1') then
            reg_out <= reg_in;
            end if;
        end if;
    end process;
end reg8;

