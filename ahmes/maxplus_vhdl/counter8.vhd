library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity counter8 is
port (clk,ld : in std_logic;
      din : in std_logic_vector(7 downto 0);
      count : out std_logic_vector(7 downto 0));
end counter8;

architecture counter8 of counter8 is
    signal t_cnt : unsigned(7 downto 0); -- internal counter signal
begin
    process (clk)
    begin
        if (rising_edge(clk)) then
            if (ld = '1') then t_cnt <= unsigned(din); -- load
            else t_cnt <= t_cnt + 1; -- incr
            end if;
        end if;
    end process;
    count <= std_logic_vector(t_cnt);
end counter8;

