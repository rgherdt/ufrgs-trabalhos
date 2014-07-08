library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ahmes_lib.all;

entity ctrl_unit is
port (clk         : in std_logic;
      reset       : in std_logic;
      flags_in    : in std_logic_vector(4 downto 0);
      opcode      : in bit8;
      control_out : out ctlcod_type;
end ctrl_unit;

architecture ctrl_unit of ctrl_unit is

    component count3 is
    port (clk,ld : in std_logic;
          din : in std_logic_vector (2 downto 0);
          count : out std_logic_vector (2 downto 0));
    end component;

    signal couter_ld : std_logic;
    signal next_stage : std_logic_vector(2 downto 0);
    signal nflag, zflag, cflag, vflag, bflag : std_logic;
    nflag <= flags_in(0);
    zflag <= flags_in(1);
    cflag <= flags_in(2);
    vflag <= flags_in(3);
    bflag <= flags_in(4);

    constant t0_cod : ctlcod_type := "000010000000000";
    constant t1_cod : ctlcod_type := "010010000000000";
    constant t2_cod : ctlcod_type := "000000001000000";
    constant t3_op  : ctlcod_type := "000010000000000";
    constant t3_not : ctlcod_type := "100000000000001";
    constant t3_brp : ctlcod_type := "010000000000000";
    constant t4_op  : ctlcod_type := "010001000000000";
    constant t4_br  : ctlcod_type := "000001000000000";
    constant t5_op  : ctlcod_type := "000110000000000";
    constant t5_br  : ctlcod_type := "001000000000000";
    constant t6_op  : ctlcod_type := "000001000000000";
    constant t6_sta : ctlcod_type := "000000010000000";
    constant t7_sta : ctlcod_type := "000000100000000";
    constant t7_lda : ctlcod_type := "100000000000001";
    constant t7_add : ctlcod_type := "100000000000001";
    constant t7_or  : ctlcod_type := "100000000000001";
    constant t7_and : ctlcod_type := "100000000000001";
    constant t7_sub : ctlcod_type := "100000000000001";

    signal aluop : aluop_type;
    signal alunop, alusta, alusta, alulda, aluadd, aluor, aluand, alunot,
           alusub alujmp, alujn, alujp, alujv, alujnv, alujz, alujnz, alujc,
           alujnc, alujb, alujnb, alushr, alushl, aluror, alurol, aluhlt;
    signal opflag, brflag : std_logic;

begin

    aluadd <= aluop(0);
    aluor  <= aluop(1);
    aluand <= aluop(2);
    alunot <= aluop(3);
    alusub <= aluop(4);

    st_counter : counter3
    port map (clk   => clk,
              ld    => counter_ld,
              din   => PS;
              count => NS);

    decode : process (clk)
    begin
        case opcode is
            when NOPCOD => aluop <= "00000";
            when ADDCOD => aluop <= "10000";
            when ORCOD  => aluop <= "01000";
            when ANDCOD => aluop <= "00100";
            when NOTCOD => aluop <= "00010";
            when SUBCOD => aluop <= "00001";
            when others => aluop <= "00000";
        end case;
    end process;
              
    uc : process (clk)
    begin
        if (reset = '1') then
            NS <= "000";
        elsif (rising_edge(clk)) then
            case PS is
                when "000" => control_out <= t0_cod;
                when "001" => control_out <= t1_cod;
                when "010" => control_out <= t2_cod;
                when "011" =>
                    if (alunot) then
                        control_out <= t3_not;
                    elsif (aluop /= "00000") then
                        control_out <= t3_op;
                    els
                when "001" => control_out <= t1_cod;
                when "010" => control_out <= t2_cod;
