library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity ahmes_ctrl_unit is
port (clk    : in std_logic;
      reset  : in std_logic;
      nflag  : in std_logic;
      zflag  : in std_logic;
      cflag  : in std_logic;
      vflag  : in std_logic;
      bflag  : in std_logic;

      decnop : in std_logic;
      decsta : in std_logic;
      declda : in std_logic;
      decadd : in std_logic;
      decor  : in std_logic;
      decand : in std_logic;
      decnot : in std_logic;
      decsub : in std_logic;
      decshr : in std_logic;
      decshl : in std_logic;
      decror : in std_logic;
      decrol : in std_logic;
      decjmp : in std_logic;
      decjn  : in std_logic;
      decjp  : in std_logic;
      decjz  : in std_logic;
      decjnz : in std_logic;
      decjc  : in std_logic;
      decjnc : in std_logic;
      decjv  : in std_logic;
      decjnv : in std_logic;
      decjb  : in std_logic;
      decjnb : in std_logic;
      dechlt : in std_logic;

      alu_add  : out std_logic; 
      alu_or   : out std_logic;  
      alu_and  : out std_logic;  
      alu_not  : out std_logic;  
      alu_sub  : out std_logic;  
      alu_py   : out std_logic; 
      ctl_shr  : out std_logic;  
      ctl_shl  : out std_logic;  
      ctl_ror  : out std_logic;  
      ctl_rol  : out std_logic;  
      ac_ld    : out std_logic;  
      flags_ld : out std_logic;  
      pc_inc   : out std_logic;  
      pc_ld    : out std_logic;  
      mpx_sel  : out std_logic;  
      rem_ld   : out std_logic;  
      mem_rd   : out std_logic;  
      mem_wr   : out std_logic;  
      rrdm_ld  : out std_logic;  
      wrdm_ld  : out std_logic;  
      ri_ld    : out std_logic);
end ahmes_ctrl_unit;

architecture ahmes_ctrl_unit of ahmes_ctrl_unit is
    subtype ctlcod_type is std_logic_vector(20 downto 0);
    subtype instdec_type is std_logic_vector(23 downto 0);

    component counter3 is
    port (clk, reset : in std_logic;
          count : out std_logic_vector (2 downto 0));
    end component;

    signal reset_counter : std_logic;
    signal control_out : std_logic_vector(20 downto 0);

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

    signal NS, PS : std_logic_vector(2 downto 0);

begin
--    control_out <= "000000000000000000000"; 

    alu_add   <= control_out(20);
    alu_or    <= control_out(19);
    alu_and   <= control_out(18);
    alu_not   <= control_out(17);
    alu_sub   <= control_out(16);
    alu_py    <= control_out(15);
    ctl_shr   <= control_out(14);
    ctl_shl   <= control_out(13);
    ctl_ror   <= control_out(12);
    ctl_rol   <= control_out(11);
    ac_ld     <= control_out(10);
    flags_ld  <= control_out(9);
    pc_inc    <= control_out(8);
    pc_ld     <= control_out(7);
    mpx_sel   <= control_out(6);
    rem_ld    <= control_out(5);
    mem_rd    <= control_out(4);
    mem_wr    <= control_out(3);
    rrdm_ld   <= control_out(2);
    wrdm_ld   <= control_out(1);
    ri_ld     <= control_out(0);

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
