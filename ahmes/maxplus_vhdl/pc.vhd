library ieee;
use ieee.std_logic_1164.all;

entity pc is
port (reg_in : in std_logic_vector(7 downto 0);
      inc, ld, clk : in std_logic;
      reg_out : out std_logic_vector(7 downto 0));
end pc;

architecture pc of pc is
begin
    variable temp : unsigned(7 downto 0);
    reg: process(clk)
    begin
        if (rising_edge(clk)) then
            if (ld = '1') then
                temp := unsigned(reg_in);
            elsif (inc = '1') then
                temp := temp + 1;
            end if;
            reg_out <= temp;
        end if;
    end process;
end pc;

