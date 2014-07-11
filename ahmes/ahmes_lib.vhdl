library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package ahmes_lib is
--    type aluop_type is (add_op, sub_op, or_op, and_op, not_op);
    type shift_type is (shift_r, shift_l, rotate_l, rotate_r);
    type branch_type is (jmp, jn, jp, jv, jnv, jz, jnz, jc, jnc, jb, jnb);
    type state_type is (nop, sta, lda, hlt);
    type memory_type is array (0 to 31) of std_logic_vector(0 to 7);
    subtype ctlcod_type is std_logic_vector(0 to 20);
    subtype instdec_type is std_logic_vector(0 to 23);
    subtype aluop_type is std_logic_vector(0 to 5);
    subtype bus8 is std_logic_vector(0 to 7);

    constant NOPCOD : std_logic_vector(0 to 7) := x"00";
    constant STACOD : std_logic_vector(0 to 7) := x"10";
    constant LDACOD : std_logic_vector(0 to 7) := x"20";
    constant ADDCOD : std_logic_vector(0 to 7) := x"30";
    constant ORCOD  : std_logic_vector(0 to 7) := x"40";
    constant ANDCOD : std_logic_vector(0 to 7) := x"50";
    constant NOTCOD : std_logic_vector(0 to 7) := x"60";
    constant SUBCOD : std_logic_vector(0 to 7) := x"70";
    constant JMPCOD : std_logic_vector(0 to 7) := x"80";
    constant JNCOD  : std_logic_vector(0 to 7) := x"90";
    constant JPCOD  : std_logic_vector(0 to 7) := x"94";
    constant JVCOD  : std_logic_vector(0 to 7) := x"98";
    constant JNVCOD : std_logic_vector(0 to 7) := x"9c";
    constant JZCOD  : std_logic_vector(0 to 7) := x"a0";
    constant JNZCOD : std_logic_vector(0 to 7) := x"a4";
    constant JCCOD  : std_logic_vector(0 to 7) := x"b0";
    constant JNCCOD : std_logic_vector(0 to 7) := x"b4";
    constant JBCOD  : std_logic_vector(0 to 7) := x"b8";
    constant JNBCOD : std_logic_vector(0 to 7) := x"bc";
    constant SHRCOD : std_logic_vector(0 to 7) := x"e0";
    constant SHLCOD : std_logic_vector(0 to 7) := x"e1";
    constant RORCOD : std_logic_vector(0 to 7) := x"e2";
    constant ROLCOD : std_logic_vector(0 to 7) := x"e3";
    constant HLTCOD : std_logic_vector(0 to 7) := x"f0";
end ahmes_lib; 

library ieee;
use ieee.std_logic_1164.all;
use work.ahmes_lib.all;

entity reg8 is
port (reg_in : in bus8;
      ld, clk : in std_logic;
      reg_out : out bus8);
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
use work.ahmes_lib.all;

entity reg5 is
port (reg_in  : in std_logic_vector(4 downto 0);
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity count8 is
port (clk,ld : in std_logic;
      din : in bus8;
      count : out bus8);
end count8;

architecture count8 of count8 is
    signal t_cnt : unsigned(0 to 7); -- internal counter signal
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
end count8;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity count3 is
port (clk,ld : in std_logic;
      din : in std_logic_vector (0 to 2);
      count : out std_logic_vector (0 to 2));
end count3;

architecture count3 of count3 is
    signal t_cnt : unsigned(0 to 2); -- internal counter signal
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
end count3;


library ieee;
use ieee.std_logic_1164.all;
use work.ahmes_lib.all;

entity mux16_8 is
port (clk, sel : in std_logic;
      mux_in0 : in bus8;
      mux_in1 : in bus8;
      mux_out : out bus8);
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity shifter is
port (clk, ld, shl, shr, rol_flag, ror_flag, cflag_in : std_logic;
      din : in std_logic_vector(0 to 7);
      dout : out std_logic_vector(0 to 7);
      nflag, zflag, cflag: out std_logic);
end shifter;

architecture shifter of shifter is
    signal temp : unsigned(0 to 8);
begin
    shifter: process(clk)
    begin
        if (rising_edge(clk)) then
            if (ld = '1') then
                temp(0 to 7) <= unsigned(din);
            elsif (shl = '1') then
                temp <= shift_left(unsigned(cflag_in & din), 1);
            elsif (shr = '1') then
                temp <= shift_right(unsigned(din & cflag_in), 1);
            elsif (rol_flag = '1') then
                temp <= rotate_left(unsigned(cflag_in & din), 1);
            elsif (ror_flag = '1') then
                temp <= rotate_right(unsigned(cflag_in & din), 1);
            end if;
            if (temp = 0) then
                zflag <= '1';
                nflag <= '0';
            elsif (temp(0) = '1') then
                zflag <= '0';
                nflag <= '1';
            else
                zflag <= '0';
                nflag <= '0';
            end if;
            cflag  <= temp(0);
            dout   <= std_logic_vector(temp(1 to 8));
        end if;
    end process;
end shifter;
                
