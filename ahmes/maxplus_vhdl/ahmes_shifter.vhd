library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity ahmes_shifter is
port (clk, ld, reset, shl_flag, shr_flag, rol_flag, ror_flag, cflag_in : std_logic;
      din : in std_logic_vector(7 downto 0);
      dout : out std_logic_vector(7 downto 0);
      nflag, zflag, cflag: out std_logic);
end ahmes_shifter;

architecture pure_shifter of ahmes_shifter is
begin
    pure_shifter: process(clk, din)
        variable temp : std_logic_vector(8 downto 0);
    begin
		if (reset = '1') then
			dout <= "00000000";
			cflag <= '0';
			nflag <= '0';
        elsif (rising_edge(clk)) then
            if (ld = '1') then
                temp := cflag_in & din;
            elsif (shl_flag = '1') then
                temp := temp(7 downto 0) & '0';
            elsif (shr_flag = '1') then
                temp := temp(0) & '0' & temp(7 downto 1);
            elsif (rol_flag = '1') then
                temp := temp(7 downto 0) & temp(8);
            elsif (ror_flag = '1') then
                temp := temp(0) & temp(8) & temp(7 downto 1);
            end if;
            if (temp(7 downto 0) = 0) then
                zflag <= '1';
            else
                zflag <= '0';
            end if;
            nflag  <= temp(7);
            cflag  <= temp(8);
            dout   <= temp(7 downto 0);
        end if;
--        else temp(7 downto 0) := din;
    end process;
end pure_shifter;

               
