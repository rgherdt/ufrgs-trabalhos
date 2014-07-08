library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;
use ieee.std_logic_arith.all;
use work.ahmes_lib.all;

entity alu is
port (a : in std_logic_vector(7 downto 0);
      b : in std_logic_vector(7 downto 0);
      sel : in std_logic_vector(1 downto 0);
      alu_out : out std_logic_vector(7 downto 0);
      n_alu, z_alu, c_alu, v_alu, b_alu : out std_logic;
end alu;

architecture behv of alu is
    signal res : std_logic_vector(7 downto 0);
    signal n, z, c, v, bflag : std_logic;
begin
    alu: process (a, b, sel)
    begin
        case sel is
            when "00" => res <= a + b;
            when "01" => res <= a + (not b) + 1;
            when "10" => res <= a and b;
            when "11" => res <= a or b;
            when others => res <= "00000000";
        end case;
        if res = 0 then z_alu <= '1'; n_alu <= '0'; c_alu <= '0'; v_alu <= '0'; b_alu <= '0';
        elsif res < 0 then z_alu <= '0'; n_alu <= '1'; c_alu <= '0'; v_alu <= '0'; b_alu <= '0';
        end if;
        alu_out <= res;
    end process;
end behv;
