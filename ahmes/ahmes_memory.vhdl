library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity memory is
port (clk    : in std_logic;
      reset  : in std_logic;
      rem_in : in bus8;
      read, write : in std_logic;
      rdm_in : in bus8;
      rdm_out : out bus8);
end memory;

architecture memory of memory is
     signal address : std_logic_vector(4 downto 0);
     signal memdata: memory_type;
begin
     process(clk, reset)
       variable i: integer;
     begin
       address <= rem_in(0 to 4);
       -- reset 
       rdm_out <= x"00";
       if (reset='1') then
           memdata(0) <= LDACOD;
           memdata(1) <= x"1F";
           memdata(2) <= STACOD;
           memdata(3) <= x"1E";
           memdata(4) <= ADDCOD;
           memdata(5) <= x"1E";
           memdata(6) <= JMPCOD;
           memdata(7) <= x"04";
           memdata(8) <= x"00";
           memdata(9) <= x"00";
           memdata(10) <= x"00";
           memdata(11) <= x"00";
           memdata(12) <= x"00";
           memdata(13) <= x"00";
           memdata(14) <= x"00";
           memdata(15) <= x"00";
           memdata(16) <= x"00";
           memdata(17) <= x"00";
           memdata(18) <= x"00";
           memdata(19) <= x"00";
           memdata(20) <= x"00";
           memdata(21) <= x"00";
           memdata(22) <= x"00";
           memdata(23) <= x"00";
           memdata(24) <= x"00";
           memdata(25) <= x"00";
           memdata(26) <= x"00";
           memdata(27) <= x"00";
           memdata(28) <= x"00";
           memdata(29) <= x"00";
           memdata(30) <= x"00";
           memdata(31) <= x"23";
       -- synchronized operations
       elsif (rising_edge(clk)) then
         -- accumulator
         if (write = '1') then
           case address is
           when "00000" => memdata(0) <= rdm_in;
           when "00001" => memdata(1) <= rdm_in;
           when "00010" => memdata(2) <= rdm_in;
           when "00011" => memdata(3) <= rdm_in;
           when "00100" => memdata(4) <= rdm_in;
           when "00101" => memdata(5) <= rdm_in;
           when "00110" => memdata(6) <= rdm_in;
           when "00111" => memdata(7) <= rdm_in;
           when "01000" => memdata(8) <= rdm_in;
           when "01001" => memdata(9) <= rdm_in;
           when "01010" => memdata(10) <= rdm_in;
           when "01011" => memdata(11) <= rdm_in;
           when "01100" => memdata(12) <= rdm_in;
           when "01101" => memdata(13) <= rdm_in;
           when "01110" => memdata(14) <= rdm_in;
           when "01111" => memdata(15) <= rdm_in;
           when "10000" => memdata(16) <= rdm_in;
           when "10001" => memdata(17) <= rdm_in;
           when "10010" => memdata(18) <= rdm_in;
           when "10011" => memdata(19) <= rdm_in;
           when "10100" => memdata(20) <= rdm_in;
           when "10101" => memdata(21) <= rdm_in;
           when "10110" => memdata(22) <= rdm_in;
           when "10111" => memdata(23) <= rdm_in;
           when "11000" => memdata(24) <= rdm_in;
           when "11001" => memdata(25) <= rdm_in;
           when "11010" => memdata(26) <= rdm_in;
           when "11011" => memdata(27) <= rdm_in;
           when "11100" => memdata(28) <= rdm_in;
           when "11101" => memdata(29) <= rdm_in;
           when "11110" => memdata(30) <= rdm_in;
           when "11111" => memdata(31) <= rdm_in;
           when others => memdata(0) <= rdm_in;
           end case;
         end if;
       end if;
     end process;

     rdm_out <=	memdata(0) when address="00000" else
                memdata(1) when address="00001" else
                memdata(2) when address="00010" else
                memdata(3) when address="00011" else
                memdata(4) when address="00100" else
                memdata(5) when address="00101" else
                memdata(6) when address="00110" else
                memdata(7) when address="00111" else
                memdata(8) when address="01000" else
                memdata(9) when address="01001" else
                memdata(10) when address="01010" else
                memdata(11) when address="01011" else
                memdata(12) when address="01100" else
                memdata(13) when address="01101" else
                memdata(14) when address="01110" else
                memdata(15) when address="01111" else
                memdata(16) when address="10000" else
                memdata(17) when address="10001" else
                memdata(18) when address="10010" else
                memdata(19) when address="10011" else
                memdata(20) when address="10100" else
                memdata(21) when address="10101" else
                memdata(22) when address="10110" else
                memdata(23) when address="10111" else
                memdata(24) when address="11000" else
                memdata(25) when address="11001" else
                memdata(26) when address="11010" else
                memdata(27) when address="11011" else
                memdata(28) when address="11100" else
                memdata(29) when address="11101" else
                memdata(30) when address="11110" else
                memdata(31) when address="11111" else
                memdata(0);
end memory;


