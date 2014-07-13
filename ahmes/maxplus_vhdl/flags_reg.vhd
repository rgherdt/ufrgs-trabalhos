library ieee;
use ieee.std_logic_1164.all;

entity flags_reg is
port (ld, clk  : in std_logic;
      nflag_in : in std_logic;
      zflag_in : in std_logic;
      cflag_in : in std_logic;
      vflag_in : in std_logic;
      bflag_in : in std_logic;
      nflag_out : out std_logic;
      zflag_out : out std_logic;
      cflag_out : out std_logic;
      vflag_out : out std_logic;
      bflag_out : out std_logic);
end flags_reg;

architecture flags_reg of flags_reg is
begin
    reg: process(clk)
        variable flags : std_logic_vector(4 downto 0);
    begin
        nflag_out <= flags(4);
        zflag_out <= flags(3);
        cflag_out <= flags(2);
        vflag_out <= flags(1);
        bflag_out <= flags(0);
        if (rising_edge(clk)) then
            if (ld = '1') then
                flags := nflag_in & zflag_in & cflag_in & vflag_in & bflag_in;
            end if;
        end if;
    end process;
end flags_reg;

