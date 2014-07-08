library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

package ahmes_lib is
    type aluop_type is (add_op, sub_op, or_op, and_op, not_op);
    type shift_type is (shift_r, shift_l, rotate_l, rotate_r);
    type branch_type is (jmp, jn, jp, jv, jnv, jz, jnz, jc, jnc, jb, jnb);
    type state_type is (nop, sta, lda, hlt);
    subtype byte is unsigned(7 downto 0);
end ahmes_lib;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity count8 is
port (clk,ld : in std_logic;
      din : in std_logic_vector (7 downto 0);
      count : out std_logic_vector (7 downto 0));
end count8;

architecture my_count of count8 is
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
end my_count;

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
            if (sel = '0') then mux_out <= mux_in0
            else mux_out <= mux_in1;
            end if;
        end if;
    end process;
end mux16_8;
