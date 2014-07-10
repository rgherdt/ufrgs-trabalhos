library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity alu is
port (x : in signed(0 to 7);
      y : in signed(0 to 7);
      alu_opsel : in std_logic_vector(0 to 5);
      alu_out : out signed(0 to 7);
      flags_out : out std_logic_vector(0 to 4));
end alu;

architecture behv of alu is
    signal res : signed(0 to 7);
    signal flags : std_logic_vector(0 to 4);
    signal alu_add, alu_or, alu_and, alu_not, alu_py, alu_sub : std_logic;
    signal nflag, zflag, cflag, vflag, bflag : std_logic;
begin
    nflag <= flags(0);
    zflag <= flags(1);
    cflag <= flags(2);
    vflag <= flags(3);
    bflag <= flags(4);
    alu_add <= alu_opsel(0);
    alu_or  <= alu_opsel(1);
    alu_and <= alu_opsel(2);
    alu_not <= alu_opsel(3);
    alu_py  <= alu_opsel(4);
    alu_sub <= alu_opsel(5);
    alu: process (x, y, alu_add, alu_or, alu_and, alu_not, alu_py, alu_sub)
    variable temp_res : signed (0 to 8); --one more due to carry flag
    begin
        temp_res := '0' & x"00";
        flags <= "00000";
        if (alu_py = '1') then temp_res := '0' & y;
        elsif (alu_add = '1') then
            temp_res := ('0' & x) + ('0' & y);
            if (x(0) = '1' and y(0) = '1' and temp_res(1) = '0') then
                vflag <= '1';
            elsif (x(0) = '0' and y(0) = '0' and temp_res(1) = '1') then
                vflag <= '1';
            else
                vflag <= '0';
            end if;
        elsif (alu_or = '1')  then temp_res := '0' & (x or y);
        elsif (alu_and = '1') then temp_res := '0' & (x and y);
        elsif (alu_not = '1') then temp_res := '0' & (not x);
        elsif (alu_sub = '1') then
            temp_res := ('0' & x) + ('0' & (not y)) + 1;
            if (x(0) = '0' and y(0) = '1' and temp_res(1) = '1') then
                vflag <= '1';
            elsif (x(0) = '1' and y(0) = '0' and temp_res(1) = '0') then
                vflag <= '1';
            else
                vflag <= '0';
            end if;
--        else temp_res := '0' & x"00";
        end if;
        if (std_logic_vector(temp_res) = "000000000") then flags <= "01000";
        elsif (temp_res(1) = '1') then flags <= "10000";
        end if;
        res <= temp_res(1 to 8);
        cflag <= temp_res(0);
        bflag <= not temp_res(0);
    end process;
    alu_out <= res;
    flags_out <= flags;
end behv;
