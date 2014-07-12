library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity ahmes_alu is
port (x : in std_logic_vector(7 downto 0);
      y : in std_logic_vector(7 downto 0);
      alu_add : in std_logic;
      alu_or  : in std_logic;
      alu_and : in std_logic;
      alu_not : in std_logic;
      alu_py  : in std_logic;
      alu_sub : in std_logic;
      alu_out : out std_logic_vector(7 downto 0);
      cflag   : out std_logic;
      vflag   : out std_logic;
      bflag   : out std_logic);
end ahmes_alu;

architecture behv of ahmes_alu is
--    signal flags : std_logic_vector(4 downto 0);

begin
        --    nzcvb
--    flags <= "00000";

    alu: process (x, y, alu_add, alu_or, alu_and, alu_not, alu_py, alu_sub)
    variable temp_res : std_logic_vector (8 downto 0); --one more due to carry flag
    begin
        cflag <= '0';
        vflag <= '0';
        bflag <= '0';
        if (alu_py = '1') then temp_res := '0' & y;
        elsif (alu_add = '1') then
            temp_res := ('0' & x) + ('0' & y);
            if (x(7) = '1' and y(7) = '1' and temp_res(7) = '0') then
                vflag <= '1';
            elsif (x(7) = '0' and y(7) = '0' and temp_res(7) = '1') then
                vflag <= '1';
            else
                vflag <= '0';
            end if;
            cflag <= temp_res(8);
        elsif (alu_or = '1')  then temp_res := '0' & (x or y);
        elsif (alu_and = '1') then temp_res := '0' & (x and y);
        elsif (alu_not = '1') then temp_res := '0' & (not x);
        elsif (alu_sub = '1') then
            temp_res := ('0' & x) + ('0' & (not y)) + 1;
            if (x(7) = '0' and y(7) = '1' and temp_res(7) = '1') then
                vflag <= '1';
            elsif (x(7) = '1' and y(7) = '0' and temp_res(7) = '0') then
                vflag <= '1';
            else
                vflag <= '0';
            end if;
            bflag <= not temp_res(8);
        end if;
--        if (std_logic_vector(temp_res(7 downto 0)) = "00000000") then zflag := '1';
--        elsif (temp_res(7) = '1') then nflag := '1'; zflag := '0';
--        else nflag := '0'; zflag := '0';
--        end if;
        alu_out <= temp_res(7 downto 0);
    end process;
end behv;
