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
      n_alu, z_alu, c_alu, v_alu, b_alu : out std_logic);
end alu;

architecture behv of alu is
    signal res : signed(7 downto 0);
    signal n, z, c, v, bflag : std_logic;
begin
    alu: process (x, y, alu_op)
    begin
        if alu_py then => res <= y;
        elsif alu_add  => res <= x + y;
        elsif alu_or   => res <= x or y;
        elsif alu_and  => res <= x and y;
        elsif alu_not  => res <= not x;
        elsif alu_sub  => res <= x + (not y) + 1;
        end if;
        if res = 0 then z_alu <= '1'; n_alu <= '0'; c_alu <= '0'; v_alu <= '0'; b_alu <= '0';
        elsif res < 0 then z_alu <= '0'; n_alu <= '1'; c_alu <= '0'; v_alu <= '0'; b_alu <= '0';
        end if;
        alu_out <= res;
    end process;
end behv;
