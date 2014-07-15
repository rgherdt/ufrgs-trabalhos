--Jucemar Monteiro
--UFRGS - PGMicro
--Circuitos Digitais 
--Memoria adaptada de: http://www.inf.ufrgs.br/~johann/circuitos/neander01.vhd

library ieee;
use ieee.std_logic_1164.all;

ENTITY mem_ahmes2 is
port(	clk: in std_logic;
		reset: in std_logic;
		mem_read: in std_logic;
		mem_write: in std_logic;
		address: in std_logic_vector(4 downto 0);
		data_in: in std_logic_vector(7 downto 0);
		data_out: out std_logic_vector(7 downto 0)
);
END mem_ahmes2; 


ARCHITECTURE Behavior OF mem_ahmes2 IS
type memarray is array (0 to 31) of std_logic_vector(7 downto 0);
signal memdata: memarray;
  -- instruction mnemonics
  constant OPNOP:	std_logic_vector(7 downto 0) := "00000000";
  constant OPSTA:	std_logic_vector(7 downto 0) := "00010000";
  constant OPLDA:	std_logic_vector(7 downto 0) := "00100000";
  constant OPADD:	std_logic_vector(7 downto 0) := "00110000";
  constant OPOR:	std_logic_vector(7 downto 0) := "01000000";
  constant OPAND:	std_logic_vector(7 downto 0) := "01010000";
  constant OPNOT:	std_logic_vector(7 downto 0) := "01100000";
  constant OPSUB:	std_logic_vector(7 downto 0) := "01110000";
  constant OPJMP:	std_logic_vector(7 downto 0) := "10000000";
  constant OPJN:	std_logic_vector(7 downto 0) := "10010000";
  constant OPJP:	std_logic_vector(7 downto 0) := "10010100";
  constant OPJV:	std_logic_vector(7 downto 0) := "10011000";
  constant OPJNV:	std_logic_vector(7 downto 0) := "10011100";
  constant OPJZ:	std_logic_vector(7 downto 0) := "10100000";
  constant OPJNZ:	std_logic_vector(7 downto 0) := "10100100";
  constant OPJC:	std_logic_vector(7 downto 0) := "10110000";
  constant OPJNC:	std_logic_vector(7 downto 0) := "10110100";
  constant OPJB:	std_logic_vector(7 downto 0) := "10111000";
  constant OPJNB:	std_logic_vector(7 downto 0) := "10111100";
  constant OPSHR:	std_logic_vector(7 downto 0) := "11100000";
  constant OPSHL:	std_logic_vector(7 downto 0) := "11100001";
  constant OPROR:	std_logic_vector(7 downto 0) := "11100010";
  constant OPROL:	std_logic_vector(7 downto 0) := "11100011";
  constant OPHLT:	std_logic_vector(7 downto 0) := "11110000";
BEGIN
memory: process(clk, reset)
    variable i: integer;
	variable outsignal: std_logic_vector(7 downto 0);
  begin
		outsignal := x"00";

    -- reset 
    if (reset='1') then
    memdata(0) <= OPLDA;
	memdata(1) <= x"19";
	memdata(2) <= OPADD;
	memdata(3) <= x"1A";
	memdata(4) <= OPROR;
	memdata(5) <= OPAND;
	memdata(6) <= x"1B";
	memdata(7) <= OPSUB;
	memdata(8) <= x"1C";
	memdata(9) <= OPADD;
	memdata(10) <= x"1D";
	memdata(11) <= OPROL;
	memdata(12) <= OPSHR;
	memdata(13) <= OPSUB;
	memdata(14) <= x"1E";
	memdata(15) <= OPADD;
	memdata(16) <= x"1F";
	memdata(17) <= OPSUB;
	memdata(18) <= x"1D";
	memdata(19) <= OPJNZ;
	memdata(20) <= x"11";
	memdata(21) <= OPHLT;
	memdata(22) <= x"00";
	memdata(23) <= x"00";
	memdata(24) <= x"00";
	memdata(25) <= x"C7";
	memdata(26) <= x"C0";
	memdata(27) <= x"03";
	memdata(28) <= x"04";
	memdata(29) <= x"01";
	memdata(30) <= x"00";
	memdata(31) <= x"0A";
    -- synchronized operations
    elsif (rising_edge(clk)) then
      -- accumulator
      if (mem_write = '1') then
		case address is
		when "00000" => memdata(0) <= data_in;
		when "00001" => memdata(1) <= data_in;
		when "00010" => memdata(2) <= data_in;
		when "00011" => memdata(3) <= data_in;
		when "00100" => memdata(4) <= data_in;
		when "00101" => memdata(5) <= data_in;
		when "00110" => memdata(6) <= data_in;
		when "00111" => memdata(7) <= data_in;
		when "01000" => memdata(8) <= data_in;
		when "01001" => memdata(9) <= data_in;
		when "01010" => memdata(10) <= data_in;
		when "01011" => memdata(11) <= data_in;
		when "01100" => memdata(12) <= data_in;
		when "01101" => memdata(13) <= data_in;
		when "01110" => memdata(14) <= data_in;
		when "01111" => memdata(15) <= data_in;
		when "10000" => memdata(16) <= data_in;
		when "10001" => memdata(17) <= data_in;
		when "10010" => memdata(18) <= data_in;
		when "10011" => memdata(19) <= data_in;
		when "10100" => memdata(20) <= data_in;
		when "10101" => memdata(21) <= data_in;
		when "10110" => memdata(22) <= data_in;
		when "10111" => memdata(23) <= data_in;
		when "11000" => memdata(24) <= data_in;
		when "11001" => memdata(25) <= data_in;
		when "11010" => memdata(26) <= data_in;
		when "11011" => memdata(27) <= data_in;
		when "11100" => memdata(28) <= data_in;
		when "11101" => memdata(29) <= data_in;
		when "11110" => memdata(30) <= data_in;
		when "11111" => memdata(31) <= data_in;
		when others => memdata(0) <= data_in;
        end case;
      end if;

	  if(mem_read = '1') then 
			case address is 
				when "00000" => outsignal :=	memdata(0);
				when "00001" => outsignal :=	memdata(1);
				when "00010" => outsignal :=	memdata(2);
				when "00011" => outsignal :=	memdata(3);
				when "00100" => outsignal :=	memdata(4);
				when "00101" => outsignal :=	memdata(5);
				when "00110" => outsignal :=	memdata(6);
				when "00111" => outsignal :=	memdata(7);
				when "01000" => outsignal :=	memdata(8);
				when "01001" => outsignal :=	memdata(9);
				when "01010" => outsignal :=	memdata(10);
				when "01011" => outsignal :=	memdata(11);
				when "01100" => outsignal :=	memdata(12);
				when "01101" => outsignal :=	memdata(13);
				when "01110" => outsignal :=	memdata(14);
				when "01111" => outsignal :=	memdata(15);
				when "10000" => outsignal :=	memdata(16);
				when "10001" => outsignal :=	memdata(17);
				when "10010" => outsignal :=	memdata(18);
				when "10011" => outsignal :=	memdata(19);
				when "10100" => outsignal :=	memdata(20);
				when "10101" => outsignal :=	memdata(21);
				when "10110" => outsignal :=	memdata(22);
				when "10111" => outsignal :=	memdata(23);
				when "11000" => outsignal :=	memdata(24);
				when "11001" => outsignal :=	memdata(25);
				when "11010" => outsignal :=	memdata(26);
				when "11011" => outsignal :=	memdata(27);
				when "11100" => outsignal :=	memdata(28);
				when "11101" => outsignal :=	memdata(29);
				when "11110" => outsignal :=	memdata(30);
				when "11111" => outsignal :=	memdata(31);
				when others => outsignal :=	memdata(0);
	  end case;
	  end if;
    end if;
    data_out <= outsignal;
  end process memory;
End Behavior;
