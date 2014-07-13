library ieee;
use ieee.std_logic_1164.all;

entity mux16_8 is
port (clk, sel : in std_logic;
      mux_in0 : in std_logic_vector(7 downto 0);
      mux_in1 : in std_logic_vector(7 downto 0);
      mux_out : out std_logic_vector(7 downto 0));
end mux16_8;

architecture mux16_8 of mux16_8 is
begin
    mux: process (clk)
    begin 
        if (rising_edge(clk)) then
            if (sel = '0') then mux_out <= mux_in0;
            else mux_out <= mux_in1;
            end if;
        end if;
    end process;
end mux16_8;

