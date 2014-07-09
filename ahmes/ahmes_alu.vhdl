library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity alu is
port (x : in signed(7 downto 0);
      y : in signed(7 downto 0);
      alu_add : in std_logic;
      alu_or  : in std_logic;
      alu_and : in std_logic;
      alu_not : in std_logic;
      alu_py  : in std_logic;
      alu_sub : in std_logic;
      alu_out : out signed(7 downto 0);
      nflag, zflag, cflag, vflag, bflag : out std_logic);
end alu;

architecture behv of alu is
    signal res : signed(7 downto 0);
    signal flags : std_logic_vector(4 downto 0);
begin
    nflag <= flags(0);
    zflag <= flags(0);
    cflag <= flags(0);
    vflag <= flags(0);
    bflag <= flags(0);
    alu: process (x, y, alu_add, alu_or, alu_and, alu_not, alu_py, alu_sub)
    variable temp_res : signed (8 downto 0); --one more due to carry flag
    begin
        if (alu_py = '1') then res <= y;
        elsif (alu_add = '1') then
            temp_res := ('0' & x) + y;
            if (x(0) = '1' and y(0) = '1' and temp_res(1) = '0') then
                vflag <= '1';
            elsif (x(0) = '0' and y(0) = '0' and temp_res(1) = '1') then
                vflag <= '1';
            else
                vflag <= '0';
            end if;
        elsif (alu_or = '1')  then temp_res := x or y;
        elsif (alu_and = '1') then temp_res := x and y;
        elsif (alu_not = '1') then temp_res := not x;
        elsif (alu_sub = '1') then
            temp_res := ('0' & x) + (not y) + 1;
            if (x(0) = '0' and y(0) = '1' and temp_res(1) = '1') then
                vflag <= '1';
            elsif (x(0) = '1' and y(0) = '0' and temp_res(1) = '0') then
                vflag <= '1';
            else
                vflag <= '0';
            end if;
        end if;
        if (temp_res = "000000000") then flags <= "01000";
        elsif (temp_res < 0) then flags <= "10000";
        end if;
        alu_out <= res;
        res <= temp_res(7 downto 0);
        cflag <= temp_res(0);
        bflag <= not temp_res(0);
    end process;
end behv;
