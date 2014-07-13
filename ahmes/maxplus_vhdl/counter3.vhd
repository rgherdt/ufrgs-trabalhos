library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;


entity counter3 is
port (clk, reset : in std_logic;
      count : out std_logic_vector (2 downto 0));
end counter3;

architecture counter3 of counter3 is
    signal t_cnt : unsigned(2 downto 0) := "000"; -- internal counter signal
begin
    process (clk, reset)
    begin
        if (reset = '1') then t_cnt <= "000";
        elsif (rising_edge(clk)) then
            t_cnt <= t_cnt + 1; -- incr
        end if;
    end process;
    count <= std_logic_vector(t_cnt);
end counter3;

