library ieee;
use ieee.std_logic_1164.all;

entity reg5 is
port (reg_in : in std_logic_vector(4 downto 0);
      ld, clk : in std_logic;
      reg_out : out std_logic_vector(4 downto 0));
end reg5;

architecture reg5 of reg5 is
begin
    reg: process(clk)
    begin
        if (rising_edge(clk)) then
            if (ld = '1') then
            reg_out <= reg_in;
            end if;
        end if;
    end process;
end reg5;

