library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity ahmes_ctrl_unit is
port (clk         : in std_logic;
      reset       : in std_logic;
      flags_in    : in std_logic_vector(4 downto 0);
      dec_in      : in std_logic_vector(23 downto 0);
      control_out : out std_logic_vector(20 downto 0));
end ahmes_ctrl_unit;

architecture ahmes_ctrl_unit of ahmes_ctrl_unit is
    subtype ctlcod_type is std_logic_vector(20 downto 0);
    subtype instdec_type is std_logic_vector(23 downto 0);

    component counter3 is
    port (clk, reset : in std_logic;
          count : out std_logic_vector (2 downto 0));
    end component;

    signal reset_counter : std_logic;
    signal next_stage : std_logic_vector(2 downto 0);
    signal nflag, zflag, cflag, vflag, bflag : std_logic;
    signal decnop, decsta, declda, decadd, decor, decand, decnot, decsub,
           decshr, decshl, decror, decrol, decjmp, decjn, decjp, decjz, decjnz,
           decjc, decjnc, decjv, decjnv, decjb, decjnb, dechlt : std_logic;

    constant t0_cod : ctlcod_type := "000000000000000100000";
    constant t1_cod : ctlcod_type := "000000000000100010000";
    constant t2_cod : ctlcod_type := "000000000000000000001"; 
                                --    098765432109876543210
    constant t3_op  : ctlcod_type := "000000000000000100000"; 
    constant t3_not : ctlcod_type := "000100000011000000000"; 
    constant t3_brp : ctlcod_type := "000000000000100000000"; 
                                --    098765432109876543210
    constant t4_op  : ctlcod_type := "000000000000100010000"; 
    constant t4_br  : ctlcod_type := "000000000000000010000"; 
                                --    098765432109876543210
    constant t5_op  : ctlcod_type := "000000000000001100000"; 
    constant t5_br  : ctlcod_type := "000000000000010000000"; 
                                --    098765432109876543210
    constant t6_op  : ctlcod_type := "000000000000000010000"; 
    constant t6_sta : ctlcod_type := "000000000000000000010"; 
                                --    098765432109876543210
    constant t7_sta : ctlcod_type := "000000000000000001000"; 
    constant t7_lda : ctlcod_type := "000001000011000000000"; 
    constant t7_add : ctlcod_type := "100000000011000000000"; 
                                --    098765432109876543210
    constant t7_or  : ctlcod_type := "010000000011000000000"; 
    constant t7_and : ctlcod_type := "001000000011000000000"; 
    constant t7_sub : ctlcod_type := "000010000011000000000"; 
                                --    098765432109876543210
    constant t7_shr : ctlcod_type := "000000100011000000000"; 
    constant t7_shl : ctlcod_type := "000000010011000000000"; 
    constant t7_ror : ctlcod_type := "001000001011000000000"; 
    constant t7_rol : ctlcod_type := "010000000111000000000"; 

    signal alunop, alusta, alulda, aluadd, aluor, aluand, alunot, alusub, 
           alujmp, alujn, alujp, alujv, alujnv, alujz, alujnz, alujc, alujnc, 
           alujb, alujnb, alushr, alushl, aluror, alurol, aluhlt : std_logic;
    signal opflag, brflag : std_logic;
    signal NS, PS : std_logic_vector(2 downto 0);

begin
    nflag  <= flags_in(4);
    zflag  <= flags_in(3);
    cflag  <= flags_in(2);
    vflag  <= flags_in(1);
    bflag  <= flags_in(0);
    decnop <= dec_in(23);
    decsta <= dec_in(22);
    declda <= dec_in(21);
    decadd <= dec_in(20);
    decor  <= dec_in(19);
    decand <= dec_in(18);
    decnot <= dec_in(17);
    decsub <= dec_in(16);
    decshr <= dec_in(15);
    decshl <= dec_in(14);
    decror <= dec_in(13);
    decrol <= dec_in(12);
    decjmp <= dec_in(11);
    decjn  <= dec_in(10);
    decjp  <= dec_in(9);
    decjz  <= dec_in(8);
    decjnz <= dec_in(7);
    decjc  <= dec_in(6);
    decjnc <= dec_in(5);
    decjv  <= dec_in(4);
    decjnv <= dec_in(3);
    decjb  <= dec_in(2);
    decjnb <= dec_in(1);
    dechlt <= dec_in(0);
                                --    012345678901234567890

    st_counter : counter3
    port map (clk   => clk,
              reset => reset_counter,
              count => NS);

              
    uc : process (clk, reset)
    begin
        if (reset = '1') then
            reset_counter <= '1';
        elsif (rising_edge(clk)) then
            reset_counter <= '0';
            case NS is
                when "000" => control_out <= t0_cod;
                when "001" => control_out <= t1_cod;
                when "010" => control_out <= t2_cod;
                when "011" =>
                    if (decsta = '1' or declda = '1' or decadd = '1' or
                        decor = '1' or decand = '1' or decsub = '1' or
                        decjmp = '1' or decshr = '1' or decshl = '1' or decrol = '1' or decror = '1') then
                        control_out <= t3_op;
                    elsif (decnot = '1') then
                        control_out <= t3_not;
                    elsif ((decjn = '1' and nflag = '1' ) or
                           (decjp = '1' and nflag = '0') or
                           (decjz = '1' and zflag = '1') or
                           (decjnz = '1' and zflag = '0') or
                           (decjc = '1' and cflag = '1') or
                           (decjnc = '1' and cflag = '0') or
                           (decjv = '1' and vflag = '1') or
                           (decjnv = '1' and vflag = '0') or
                           (decjb = '1' and bflag = '1') or
                           (decjnb = '1' and bflag = '0')) then
                        control_out <= t3_op;
                    elsif  (decnop = '1' or dechlt = '1') then
                        reset_counter <= '1';
                    else
                        control_out <= t3_brp;
                        reset_counter <= '1';
                    end if;
                when "100" =>
                    if (decsta = '1' or declda = '1' or decadd = '1' or
                        decor = '1' or decand = '1' or decsub = '1' or
                        decshr = '1' or decshl = '1' or decrol = '1' or decror = '1') then
                        control_out <= t4_op;
                    else control_out <= t4_br;
                    end if;
                when "101" =>
                    if (decsta = '1' or declda = '1' or decadd = '1' or
                        decor = '1' or decand = '1' or decsub = '1' or
                        decshr = '1' or decshl = '1' or decrol = '1' or decror = '1') then
                        control_out <= t5_op;
                    else
                        control_out <= t5_br;
                        reset_counter <= '1';
                    end if;
                when "110" =>
                    if (decsta = '1') then
                        control_out <= t6_sta;
                    else control_out <= t6_op;
                    end if;
                when "111" =>
                    if (decsta = '1') then
                        control_out <= t7_sta;
                    elsif (declda = '1') then
                        control_out <= t7_lda;
                    elsif (decadd = '1') then
                        control_out <= t7_add;
                    elsif (decor = '1') then
                        control_out <= t7_or;
                    elsif (decand = '1') then
                        control_out <= t7_and;
                    elsif (decsub = '1') then
                        control_out <= t7_sub;
                    elsif (decshl = '1') then
                        control_out <= t7_sub;
                    else control_out <= (others => '0');
                    end if;
                    reset_counter <= '1';
                when others =>
                    control_out <= (others => '0');
            end case;
        end if;
    end process;
end ahmes_ctrl_unit;
