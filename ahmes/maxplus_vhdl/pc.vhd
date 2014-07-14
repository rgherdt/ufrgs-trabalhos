library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity pc is
port (din : in std_logic_vector(7 downto 0);
      inc, ld, clk : in std_logic;
      dout : out std_logic_vector(7 downto 0));
end pc;

architecture pc of pc is
begin
    reg: process(clk)
    variable temp : unsigned(7 downto 0);
    begin
        if (rising_edge(clk)) then
            if (ld = '1') then
                temp := unsigned(din);
            elsif (inc = '1') then
                temp := temp + 1;
            end if;
            dout <= std_logic_vector(temp);
        end if;
    end process;
end pc;

