library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity alu is
port (x : in unsign8;
      y : in unsign8;
      alu_opsel : in std_logic_vector(5 downto 0);
      alu_out : out unsign8;
      flags_out : out std_logic_vector(4 downto 0));
end alu;

architecture behv of alu is
    signal res : unsign8;
    signal alu_add, alu_or, alu_and, alu_not, alu_py, alu_sub : std_logic;
--    signal flags : std_logic_vector(4 downto 0);

begin
    alu_add <= alu_opsel(5);
    alu_or  <= alu_opsel(4);
    alu_and <= alu_opsel(3);
    alu_not <= alu_opsel(2);
    alu_py  <= alu_opsel(1);
    alu_sub <= alu_opsel(0);
        --    nzcvb
--    flags <= "00000";

    alu: process (x, y, alu_add, alu_or, alu_and, alu_not, alu_py, alu_sub)
    variable temp_res : unsigned (8 downto 0); --one more due to carry flag
    variable nflag, zflag, cflag, vflag, bflag : std_logic;
    begin
        nflag := '0';
        zflag := '0';
        cflag := '0';
        vflag := '0';
        bflag := '0';

        if (alu_py = '1') then res <= y;
        elsif (alu_add = '1') then
            temp_res := ('0' & x) + ('0' & y);
            if (x(7) = '1' and y(7) = '1' and temp_res(7) = '0') then
                vflag := '1';
            elsif (x(7) = '0' and y(7) = '0' and temp_res(7) = '1') then
                vflag := '1';
            else
                vflag := '0';
            end if;
            cflag := temp_res(8);
        elsif (alu_or = '1')  then temp_res := x or y;
        elsif (alu_and = '1') then temp_res := x and y;
        elsif (alu_not = '1') then temp_res := not x;
        elsif (alu_sub = '1') then
            temp_res := ('0' & x) + ('0' & (not y)) + 1;
            if (x(7) = '0' and y(7) = '1' and temp_res(7) = '1') then
                vflag := '1';
            elsif (x(7) = '1' and y(7) = '0' and temp_res(7) = '0') then
                vflag := '1';
            else
                vflag := '0';
            end if;
            bflag := not temp_res(8);
        end if;
--        if (std_logic_vector(temp_res(7 downto 0)) = "00000000") then zflag := '1';
--        elsif (temp_res(7) = '1') then nflag := '1'; zflag := '0';
--        else nflag := '0'; zflag := '0';
--        end if;
        alu_out <= temp_res(7 downto 0);
        flags_out <= nflag & zflag & cflag & vflag & bflag;
    end process;
end behv;
