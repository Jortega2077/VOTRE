      **********************************************************************                        
      ** Programa     : VTPSGBBR                                          **                        
      ** Descripciòn: : Pg Inscripciones ó Re-Incorporaciones Rentables   **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Creaciòn: 25 de Marzo de 2016                              **                        
      ** IEGINO (Indicador de origen)=  '1' icorporaciones rentables      **                        
      **********************************************************************                        
     HOption(*NoDebugIo)                                                                            
     HDFTACTGRP(*NO) Bnddir('VTMGOI12R')                                                            
                                                                                                    
       /Define CountUsingSql                                                                        
       /Include Votrep00/Qrpglesrc,Vtmgoi14r                                                        
       /Undefine CountUsingSql                                                                      
                                                                                                    
     ** Definiciòn de Archivo de Trabajo                                                            
     FVtwiegf   Uf A e           k Disk                                                             
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
     DPrInsuficienciaCte...                                                                         
     D                 Pr             9  0                                                          
     DFields         e Ds                  Extname(Vtwiegf:Regieg)                                  
     D                                     Prefix(Q)                                                
     DInfHistLider     Ds                                                                           
     DWlmgcia                         3A                                                            
     DWlmgmed                         4A                                                            
     DWlmgzon                         3A                                                            
     DWlmgsec                         3A                                                            
     DWlmgced                        15  0                                                          
     DWlmgcod                         9  0                                                          
     DWLmgtfa                        11  2                                                          
     DWlmgnir                        10A                                                            
     DWlmgbme                         4A                                                            
     DWlmgnci                         3  0                                                          
     DWlmgcla                        10A                                                            
     DWlmgtvp                        11  2                                                          
     DWlmgome                         4A                                                            
     DWlmgime                         1A                                                            
     DWGldcte                        15  0                                                          
     DWgldsct                         1A                                                            
                                                                                                    
      ** Defime estructuras de trabajo                                                              
      * Estructura recupera valor parametrizado Rentabilidad                                        
       Dcl-ds DsValrRenta Dim(999) Qualified;                                                       
            OcodMod Packed(3);                                                                      
            ONumPed Packed(3);                                                                      
            OVlrPed Packed(11:2);                                                                   
       End-ds;                                                                                      
                                                                                                    
      * Estructura recupera valor de pedidos                                                        
       Dcl-Ds DsVlrTtlPlan Dim(999) Qualified;                                                      
         dscmp Char(4);                                                                             
         dsVlr Zoned(15:2);                                                                         
       End-Ds DsVlrTtlPlan;                                                                         
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DIcia                            3a                                                            
     DICamPa                          4A                                                            
     DIZona                           3a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DIcia                            3a                                                            
     DICamPa                          4A                                                            
     DIZona                           3a                                                            
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
                                                                                                    
      * Definiciòn de Vectores                                                                      
                                                                                                    
      *Definición de Procedimientos                                                                 
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPCia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
                                                                                                    
      * Programa validar region                                                                     
     DPgmregio         Pr                  Extpgm('VOTREP00/VTMGOIGR')                              
     DIcia                            3a                                                            
     DIzona                           3a                                                            
     DORegion                        10a                                                            
     DOced                           10a                                                            
     DOplan                          10a                                                            
                                                                                                    
      * Programa valida las N Campañas                                                              
     DPgmNCampa        Pr                  Extpgm('VOTREP00/VTMPGBBR')                              
     DIcia                            3a                                                            
     DIRegion                        10a                                                            
     DIzona                           3a                                                            
     DIsector                         3a                                                            
     DICamPa                          4a                                                            
     DOnro                            4  0                                                          
                                                                                                    
     ** Consulta efectividad de Pedido                                                              
     DPgmEfectividad   Pr                  Extpgm('VOTREP00/VTPEFE0R')                              
     DXCia                            3                                                             
     DXCed                            9  0                                                          
     DXOrd                            9  0                                                          
     DXEfe                            1a                                                            
                                                                                                    
     DValZonaMulPed    Pr                  Extpgm('VTPOLP00/VPCONSU02R')                            
     D P_Cia                          3a                                                            
     D P_Zon                          3a                                                            
     D P_IndZon                       1a                                                            
                                                                                                    
      * programa valida Valor facturado                                                             
       Dcl-Pr PgmVlrInsRent  Extpgm('VOTREP00/VTMPGBUR');                                           
        Xcia     Char(3);                                                                           
        Xreg     Char(30);                                                                          
        Xzon     Char(3);                                                                           
        Xsec     Char(3);                                                                           
        Xctacond Zoned(3);                                                                          
        Xoval LikeDs(DsValrRenta) Dim(999);                                                         
       End-Pr;                                                                                      
                                                                                                    
      * programa Valor a Pagar Inscripciones Efectivas Guías                                        
     DPgmConsvalor     Pr                  Extpgm('VOTREP00/VTMPGBAR')                              
     DYcia                            3                                                             
     DYreg                           30                                                             
     DYzon                            3                                                             
     DYsec                            3                                                             
     DYcam                            4                                                             
     DYoval                          11  2                                                          
                                                                                                    
      *Definición de variables de trabajo                                                           
     D P_IndZon        s              1a                                                            
     DPCamEa           S              4a   Inz(*Blanks)                                             
     DPCamRa           S              4a   Inz(*Blanks)                                             
     DPCamRa1          S              4a   Inz(*Blanks)                                             
     DPOpera           S              1a   Inz(*Blanks)                                             
     DPnumea           S              3a   Inz(*Blanks)                                             
     DKcam             s              4a                                                            
     DWApli            s              1a                                                            
     Dcia              s              3a                                                            
     DXcia             s              3    Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DWsql             S           1000A                                                            
     DSqlStr           s           2000a   Inz(*Blanks)                                             
     DSqlStr0          s           2000a   Inz(*Blanks)                                             
     DSqlStr1          s           2000a   Inz(*Blanks)                                             
     D$COMA            C                   ''''                                                     
     DPCia             S              3a   Inz(*Blanks)                                             
     DPCamE            S              4a   Inz(*Blanks)                                             
     DPCamR            S              4a   Inz(*Blanks)                                             
     DPOper            S              1a   Inz(*Blanks)                                             
     DPnume            S              3a   Inz(*Blanks)                                             
     DUser             s             10a   Inz(*User)                                               
     DWvco             s              1a   Inz(*Blanks)                                             
     DW_I              s              1a   Inz(*Blanks)                                             
     DW_R              s              1a   Inz(*Blanks)                                             
     DIRegion          S             30a   Inz(*Blanks)                                             
     DXced             S             10a   Inz(*Blanks)                                             
     DXplan            S             10a   Inz(*Blanks)                                             
     DNCAMPA           S              9  0 Inz(*Zeros)                                              
     DIsector          S              3A   Inz(*Blanks)                                             
     DOnro             S              4  0 Inz(*Zeros)                                              
     DWOhmeda          S             10A   Inz(*Blanks)                                             
     DWohordÑ          S              9  0 Inz(*Zeros)                                              
     DWOhrcst          S              1A   Inz(*Blanks)                                             
     DCampaAux         S             10A   Inz(*Blanks)                                             
     DICedula          S             15  0 Inz(*Zeros)                                              
     DOREGION          S             10A   Inz(*Blanks)                                             
     DWOhcstÑ          S              9  0 Inz(*Zeros)                                              
     DW_Orden          S              9  0 Inz(*Zeros)                                              
     DW_Efetividad     S              1A   Inz(*Blanks)                                             
     DWIndEfectivi     S              1  0 Inz(*Zeros)                                              
     DOValor           S             11  2 Inz(*Zeros)                                              
     DXreg             S             30A   Inz(*Blanks)                                             
     DWValor           S             12  2 Inz(*Zeros)                                              
     DXCampaÑ          S              4A   Inz(*Blanks)                                             
     DPricampa         S              4A   Inz(*Blanks)                                             
     DW_USER           s             10a   Inz(*User)                                               
     DQnro             S              4  0 Inz(*Zeros)                                              
     DYreg             S             30A   Inz(*Blanks)                                             
     DYsec             S              3    Inz(*Blanks)                                             
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DIcedulaX         s             30a   Inz(*Blanks)                                             
     DOrespuesta       s              1a   Inz(*Blanks)                                             
     DTfecha           s             10a   Inz(*Blanks)                                             
     DWcedcom          S              9  0 Inz(*Zeros)                                              
     DWsect            s              1a   Inz(*Blanks)                                             
       Dcl-s PciaV Char(3) Inz('999');                                                              
       Dcl-s Xrol Char(2) Inz('CM');                                                                
       Dcl-s Xfec Char(10) Inz(*Blanks);                                                            
       Dcl-s Xtexto Char(200) Inz(*Blanks);                                                         
       Dcl-s w_campanhaTmp char(4);                                                                 
       Dcl-s Elements  Int(5) Inz(%Elem(DsVlrTtlPlan));                                             
       Dcl-s W_NroPeds Zoned(3);                                                                    
       Dcl-s Idx Zoned(3);                                                                          
       Dcl-s IdxN Zoned(3);                                                                         
       Dcl-s dx1 Zoned(4);                                                                          
       Dcl-S SearchString Char(1000);                                                               
       Dcl-S Count Packed(10);                                                                      
       Dcl-S CountM Packed(10);                                                                     
       Dcl-S Count1 Packed(10);                                                                     
       Dcl-S Count2 Packed(10);                                                                     
         // Indicador de origen----Reincorporaciones rentables                                      
       Dcl-s w_iegino Char(2) Inz('1') ;                                                            
                                                                                                    
     DPgmNewPlanG      Pr                  Extpgm('VOTREP00/VTMPGB0R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcaso                           7a                                                            
     DIcedulaX                       30a                                                            
     DIfecha                         10a                                                            
     DOrespuesta                      1a                                                            
                                                                                                    
       Dcl-Pr PgmMigracion Extpgm('VOTREP00/VTPLDITR');                                             
        Xcia Char(3);                                                                               
        Xrol Char(2);                                                                               
        Xfec Char(10);                                                                              
        Xtexto Char(200);                                                                           
       End-Pr;                                                                                      
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
        //Resta de la  Campaña Cierre de Zona Menos 5 Campañas                                      
           PCamE = ICamPa;                                                                          
           PCamR = *Blanks;                                                                         
           POper = '-';                                                                             
           Pnume = '1';                                                                             
           If ICamPa = '2010';                                                                      
            Pnume = '2';                                                                            
           Endif;                                                                                   
           callp(E) ProCampa(Icia:PCamE:PCamR:POper:Pnume);                                         
           PCamRa1 = *Blanks;                                                                       
           PCamRa1 = PCamR;                                                                         
                                                                                                    
       // Se valida con el Stencil de la Campaña de Cierre las                                      
       // Inscripciones o Reincorporaciones de la Campaña Anterior                                  
       // Sin Evaluar la Zona y el Sector                                                           
       SqlStr = *Blanks;                                                                            
       SqlStr0 = *Blanks;                                                                           
       SqlStr  = '(Select Distinct Lmgcod ' +                                                       
                'From votrea00/Vtglmgf Inner Join Votrea00/Vtmgldf On ' +                           
                'Lmgcia = Gldcia ' +                                                                
                'Where Lmgcia = ' + $Coma + %Trim(ICia) + $Coma + ' And ' +                         
                'Lmgmed = ' + $Coma + %Trim(PCamRa1) + $Coma + ' And ' +                            
                'Gldsct <> ' + '''' + ' ' + '''' + ' And ' +                                        
                'Lmgnir In ( ''I'' , ''R'' ))';                                                     
       SqlStr0 = 'Select Distinct Lmgcod, Gldsct ' +                                                
                'From votrea00/Vtglmgf Inner Join Votrea00/Vtmgldf On ' +                           
                'Lmgcia = Gldcia and Lmgzon = Gldzon and Lmgsec = Gldsct ' +                        
                'Where Lmgcia = ' + $Coma + %Trim(ICia) + $Coma + ' And ' +                         
                'Lmgmed = ' + $Coma + %Trim(ICamPa) + $Coma + ' And ' +                             
                'Lmgzon = ' + $Coma + %Trim(Izona) + $Coma + ' And ' +                              
                'Gldsct <> ' + '''' + ' ' + '''' + ' And ' +                                        
                '((Gldest= '' '' And Gldcin <= ' + '''' + PCamRa1 + '''' +                          
                ') Or (Gldest = ''D''' + ' And Gldcin <= ' + '''' +                                 
                 PCamRa1 + '''' + ' And (Gldcdt = ' +                                               
                 '''' + %Trim(Icampa) + '''' + ' Or Gldcdt = ' + '''' +                             
                PCamRa1 + '''' + ' Or Gldcdt >= ' + '''' + PCamRa1 + '''' +                         
                '))) And Lmgcod in ' + %Trim(SqlStr) +                                              
                ' Order By Gldsct';                                                                 
                                                                                                    
           Exec Sql                                                                                 
            Prepare Cur6e From :SqlStr0;                                                            
                                                                                                    
           Exec Sql                                                                                 
            Declare Cursor6e Cursor For Cur6e;                                                      
                                                                                                    
           Exec Sql                                                                                 
            Open Cursor6e;                                                                          
                                                                                                    
           Exec Sql                                                                                 
            Fetch Cursor6e Into :Wcedcom, :Wsect;                                                   
                                                                                                    
           Dow Sqlcod <> 100 And SqlCod >= *Zeros;                                                  
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Distinct Lmgcia, Lmgmed, Lmgzon, Lmgsec, ' +                                
                'Lmgced, Lmgcod, Lmgtfa, Lmgnir, Lmgbme, Lmgnci, ' +                                
                'Lmgcla, Lmgtvp, Lmgome, Lmgime, Gldcte, gldsct ' +                                 
                'From votrea00/Vtglmgf Inner Join Votrea00/Vtmgldf On ' +                           
                'Lmgcia = Gldcia and Lmgzon = Gldzon ' +                                            
                'Where Lmgcia = ' + $Coma + %Trim(ICia) + $Coma + ' And ' +                         
                'Lmgmed = ' + $Coma + %Trim(PCamRa1) + $Coma + ' And ' +                            
                'Lmgzon = ' + $Coma + %Trim(Izona) + $Coma + ' And ' +                              
                'Gldsct <> ' + '''' + ' ' + '''' + ' And ' +                                        
                'Lmgnir In ( ''I'' , ''R'' ) And ' +                                                
                '((Gldest= '' '' And Gldcin <= ' + '''' + PCamRa1 + '''' +                          
                ') Or (Gldest = ''D''' + ' And Gldcin <= ' + '''' +                                 
                 PCamRa1 + '''' + ' And (Gldcdt = ' +                                               
                 '''' + %Trim(Icampa) + '''' + ' Or Gldcdt = ' + '''' +                             
                 PCamRa1 + '''' + ' Or Gldcdt >= ' + '''' + PCamRa1 + '''' +                        
                 '))) And Lmgcod = ' +                                                              
                 %Trim(%Editc(Wcedcom:'Z')) + ' And Gldsct = ' + '''' +                             
                 Wsect + '''';                                                                      
                                                                                                    
           Exec Sql                                                                                 
            Prepare Cur6 From :SqlStr;                                                              
                                                                                                    
           Exec Sql                                                                                 
            Declare Cursor6 Cursor For Cur6;                                                        
                                                                                                    
           Exec Sql                                                                                 
            Open Cursor6;                                                                           
                                                                                                    
           Exec Sql                                                                                 
            Fetch Cursor6 Into :InfHistLider;                                                       
                                                                                                    
            CallP(e) Pgmregio(ICia:IZona:ORegion:Xced:xplan);                                       
             Onro = 0;                                                                              
             IRegion = ORegion;                                                                     
          Dow Sqlcod= *Zeros;                                                                       
                                                                                                    
           Icaso = *Blanks;                                                                         
           IcedulaX = %Trim(%Editc(WGldcte:'Z'));                                                   
           Ifecha = '777777';                                                                       
           Orespuesta = *Blanks;                                                                    
           PgmNewPlanG(Icia:Izona:Icaso:IcedulaX:Ifecha:Orespuesta);                                
           Tfecha = %Char(%Date());                                                                 
           Kcam = *Blanks;                                                                          
           PgmMigracion(PciaV:Xrol:Xfec:Xtexto);                                                    
           If Xfec = *Blanks;                                                                       
            Exec Sql                                                                                
             Select Max(Substr(Digits(Izpano), 3, 2) ||                                             
                   Digits(Izpcap)) Into :Kcam                                                       
             From Vpcopa00/Vtmizpf                                                                  
             Where Izpcia = :Icia And                                                               
                   Izpzon = '998' And                                                               
                   Substr(Char(Izpftp, Iso), 1, 4) || '-' ||                                        
                   Substr(Char(Izpftp, Iso), 6, 2) || '-' ||                                        
                   Substr(Char(Izpftp, Iso), 9, 2) <= :Ifecha                                       
              Fetch First 1 Row Only;                                                               
           Else;                                                                                    
            Exec Sql                                                                                
             Select Fcfcmp Into :Kcam                                                               
             From Vpcopa00/Vtcmfcf                                                                  
             Where Fcfcia = :Icia And                                                               
                   :Ifecha Between Fcffin And Fcfffn                                                
             Fetch First 1 Row Only;                                                                
           Endif;                                                                                   
          If Kcam <> *Blanks;                                                                       
           PCamEa = Kcam;                                                                           
           PCamRa = *Blanks;                                                                        
           POpera = '+';                                                                            
           Pnumea = '1';                                                                            
           If Kcam = '2008' And ICamPa = '2010';                                                    
            Pnumea = '2';                                                                           
           Endif;                                                                                   
           callp(E) ProCampa(Icia:PCamEa:PCamRa:POpera:Pnumea);                                     
           Kcam = PCamRa;                                                                           
          Endif;                                                                                    
         //If PCamR >= Kcam;                                                                        
           If PCamR >= PCamEa;                                                                      
            If Orespuesta = 'S';                                                                    
             CallP(e) PgmNCampa(ICia:IRegion:IZona:Wlmgsec:ICampa:Onro);                            
             NCampa=Onro;                                                                           
              Pricampa = *Blanks;                                                                   
             If (Wlmgnir = 'I' or (Wlmgnir='R' And                                                  
                                  (Wlmgnci >= Ncampa Or Wlmgnci = 0)));                             
              //Suma la Campaña Cierre de la Zona más 1 campaña                                     
              PCamE = Wlmgome;                                                                      
              PCamR = *Blanks;                                                                      
              POper = '+';                                                                          
              Pnume = '1';                                                                          
              If Wlmgome = '2008' And ICamPa = '2010';                                              
               Pnume = '2';                                                                         
              Endif;                                                                                
              callp(E) ProCampa(Icia:PCamE:PCamR:POper:Pnume);                                      
              CampaAux = *Blanks;                                                                   
              ICedula = Wlmgcod;                                                                    
              // Cambio Politica de Miltiples Pedidos                                               
              Clear CountM;                                                                         
              Clear SearchString;                                                                   
              SearchString = 'Select Count(0) From Opf' + ICia + '/Porhdr ' +                       
                             'Where OhcstÑ = ' + %Trim(%Char(ICedula)) +                            
                             ' And Ohmeda = ' + $Coma + %Trim(WLmgome) + $Coma +                    
                             ' And Ohcmfl= ' + '''' + 'N' + '''';                                   
              CountM = CountUsingSql(SearchString) ;                                                
              If CountM >= 2;                                                                       
               Clear Count1;                                                                        
               Clear SearchString;                                                                  
               SearchString = 'Select Count(0) From Opf' + ICia + '/Porhdr '+                       
                              'Where OhcstÑ = ' + %Trim(%Char(ICedula)) +                           
                             ' And Ohmeda = ' + $Coma + %Trim(WLmgome) + $Coma +                    
                             ' And Ohcmfl= ' + '''' + 'N' + '''' +                                  
                             ' And Ohrcst= ' + '''' + 'S' + '''';                                   
               Count1 = CountUsingSql(SearchString) ;                                               
                                                                                                    
               Clear Count2;                                                                        
               Clear SearchString;                                                                  
               SearchString = 'Select Count(Distinct(OhordÑ)) ' +                                   
                 'From Opf' + ICia + '/Porhdr Inner Join ' +                                        
                 'Opf' + ICia + '/Pordtl On OhordÑ = OdordÑ ' +                                     
                 'Where Ohsls1 = ' + '''' + Izona + '''' + ' And Ohmeda = ' +                       
                 '''' +WLmgome + '''' + ' And ' +                                                   
                 'OhcstÑ = ' + %Trim(%Char(ICedula)) + ' ' +                                        
                 'And Ohcmfl = ''N''' + ' And Ohrcst = ''C''' + ' And ' +                           
                 '(Odlccd = ''INS''' + ' Or ' + ' Odlccd = ''BAC''' +                               
                 ' Or ' + ' Odlccd = ''CCD''' + ' Or ' +                                            
                 'Odlccd = ''IN8''' + ')';                                                          
               Count2 = CountUsingSql(SearchString) ;                                               
              Endif;                                                                                
              If (Count1 + Count2) >= 2;                                                            
               Count = (Count1 + Count2);                                                           
              Else;                                                                                 
               Count = 1;                                                                           
              Endif;                                                                                
              If CountM <=1;                                                                        
               Count = 1;                                                                           
              Endif;                                                                                
                                                                                                    
              Clear P_IndZon;                                                                       
              ValZonaMulPed(Icia:Izona:P_IndZon);                                                   
              If Count >= 2 And P_IndZon = 'N';                                                     
               Count = 1;                                                                           
              Endif;                                                                                
                                                                                                    
              If Count < 2 ;                                                                        
               PrBusOrden();                                                                        
              Else;                                                                                 
               PrBusOrdenMpdos();                                                                   
              Endif;                                                                                
             EndIf;                                                                                 
            EndIf;                                                                                  
           EndIf;                                                                                   
           Exec Sql                                                                                 
            Fetch Cursor6 Into :InfHistLider ;                                                      
          EndDo;                                                                                    
                                                                                                    
           Exec Sql                                                                                 
            Close Cursor6;                                                                          
                                                                                                    
           Exec Sql                                                                                 
            Fetch Cursor6e Into :Wcedcom, :Wsect;                                                   
          EndDo;                                                                                    
                                                                                                    
           Exec Sql                                                                                 
            Close Cursor6e;                                                                         
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento Selección Registros ha Procesar                    /                         
      //------------------------------------------------------------------/                         
     PPrBusOrden       B                                                                            
     DPrBusOrden       Pi                                                                           
     DTcampa           s              4a                                                            
     Dw_cantCondi      s              3s 0                                                          
     DW_EfetividadP    s              1A   Inz(*Blanks)                                             
      /Free                                                                                         
       Clear W_EfetividadP;                                                                         
       Tcampa = WLmgome;                                                                            
       CallP(e) PgmVlrInsRent(ICia:Xreg:IZona:ISector:w_cantCondi:DsValrRenta);                     
       If w_cantCondi > 1;                                                                          
        PCamE = WLmgome;                                                                            
        PCamR = *Blanks;                                                                            
        POper = '+';                                                                                
        Pnume = %Trim(%Editc((w_cantCondi-1):'K'));                                                 
        If Wlmgome = '2008' And ICamPa = '2010' And %Trim(Pnume) = '1';                             
         Pnume = '2';                                                                               
        Endif;                                                                                      
        Callp(E) ProCampa(Icia:PCamE:PCamR:POper:Pnume);                                            
        Tcampa = PCamR;                                                                             
       Endif;                                                                                       
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Ohmeda, OhordÑ, Ohrcst, OhcstÑ ' +                                          
                'From Opf' + %Trim(ICia) + '/Porhdr Inner Join Opf' +                               
                %Trim(ICia) +'/Lmllst01 on OhcstÑ=MlcstÑ And ' +                                    
                'Ohmeda Between ' + $Coma + %Trim(WLmgome) + $Coma + ' And ' +                      
                                    $Coma + %Trim(Tcampa)   + $Coma +                               
                ' Where OhcstÑ = ' + %Trim(%Char(ICedula)) + ' And ' +                              
                'Ohcmfl= ''N'' ' +                                                                  
                'Order By Ohmeda Asc';                                                              
                                                                                                    
           Exec Sql                                                                                 
            Prepare Cur2 From :SqlStr;                                                              
                                                                                                    
           Exec Sql                                                                                 
            Declare Cursor2 Cursor For Cur2;                                                        
                                                                                                    
           Exec Sql                                                                                 
            Open Cursor2;                                                                           
                                                                                                    
           Exec Sql                                                                                 
           Fetch Cursor2 Into :WOhmeda, :WOhordÑ, :WOhrcst, :WOhcstÑ;                               
            WIndEfectivi=0;                                                                         
                                                                                                    
          Dow  Sqlcod= *Zeros;                                                                      
            IF WOhrcst = 'S';                                                                       
              IF WOhmeda <> CampaAux;                                                               
                 CampaAux = WOhmeda;                                                                
                 Pricampa = %Trim(WOhmeda);                                                         
                 If WIndEfectivi=0;                                                                 
                    PgmEfectividad(ICia:WOhcstÑ:WOhordÑ:W_Efetividad);                              
                    If W_EfetividadP = ' ' And W_Efetividad = 'S';                                  
                     W_EfetividadP = W_Efetividad;                                                  
                    Endif;                                                                          
                    WIndEfectivi=1;                                                                 
                    IdxN += 1;                                                                      
                 Endif;                                                                             
              EndIF;                                                                                
            EndIF;                                                                                  
            IF WOhrcst = 'C';                                                                       
            Qnro = PrInsuficienciaCte();                                                            
             If Qnro > 0;                                                                           
              Pricampa = %Trim(WOhmeda);                                                            
              IF WOhmeda <> CampaAux;                                                               
               CampaAux = WOhmeda;                                                                  
               W_Efetividad = 'S';                                                                  
               If W_EfetividadP = ' ' And W_Efetividad = 'S';                                       
                W_EfetividadP = W_Efetividad;                                                       
               Endif;                                                                               
               WIndEfectivi=1;                                                                      
               IdxN += 1;                                                                           
              EndIf;                                                                                
             EndIf;                                                                                 
            EndIF;                                                                                  
           Exec Sql                                                                                 
           Fetch Cursor2 Into :WOhmeda, :WOhordÑ, :WOhrcst, :WOhcstÑ;                               
          EndDo;                                                                                    
                                                                                                    
          If W_Efetividad = 'S' And IdxN >= Count;                                                  
           Xreg = *blanks;                                                                          
           Xreg = IRegion;                                                                          
           PrEscribeRentable();                                                                     
          Else;                                                                                     
           WApli = 'P';                                                                             
           PrGrabar();                                                                              
          EndIf;                                                                                    
                                                                                                    
           Exec Sql                                                                                 
            Close Cursor2;                                                                          
                                                                                                    
      /End-Free                                                                                     
     PPrBusOrden       E                                                                            
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento Selección Registros ha Procesar Multiples Pedidos  /                         
      //------------------------------------------------------------------/                         
     PPrBusOrdenMpdos  B                                                                            
     DPrBusOrdenMpdos  Pi                                                                           
     DTcampa           s              4a                                                            
     Dw_cantCondi      s              3s 0                                                          
     DW_EfetividadP    s              1a   Inz(*Blanks)                                             
     DYYwSW            s               n   Inz(*Off)                                                
      /Free                                                                                         
       Clear W_EfetividadP;                                                                         
       Tcampa = WLmgome;                                                                            
       CallP(e) PgmVlrInsRent(ICia:Xreg:IZona:ISector:w_cantCondi:DsValrRenta);                     
       If w_cantCondi > 1;                                                                          
        PCamE = WLmgome;                                                                            
        PCamR = *Blanks;                                                                            
        POper = '+';                                                                                
        Pnume = %Trim(%Editc((w_cantCondi-1):'K'));                                                 
        If Wlmgome = '2008' And ICamPa = '2010' And %Trim(Pnume) = '1';                             
         Pnume = '2';                                                                               
        Endif;                                                                                      
        Callp(E) ProCampa(Icia:PCamE:PCamR:POper:Pnume);                                            
        Tcampa = PCamR;                                                                             
       Endif;                                                                                       
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Ohmeda, OhordÑ, Ohrcst, OhcstÑ ' +                                          
                'From Opf' + %Trim(ICia) + '/Porhdr Inner Join Opf' +                               
                %Trim(ICia) +'/Lmllst01 on OhcstÑ=MlcstÑ And ' +                                    
                'Ohmeda Between ' + $Coma + %Trim(WLmgome) + $Coma + ' And ' +                      
                                    $Coma + %Trim(Tcampa)   + $Coma +                               
                ' Where OhcstÑ = ' + %Trim(%Char(ICedula)) + ' And ' +                              
                'Ohcmfl= ''N'' ' +                                                                  
                'Order By Ohmeda Asc';                                                              
                                                                                                    
           Exec Sql                                                                                 
            Prepare Cur2mp From :SqlStr;                                                            
                                                                                                    
           Exec Sql                                                                                 
            Declare Cursor2mp Cursor For Cur2mp;                                                    
                                                                                                    
           Exec Sql                                                                                 
            Open Cursor2mp;                                                                         
                                                                                                    
           Exec Sql                                                                                 
           Fetch Cursor2mp Into :WOhmeda, :WOhordÑ, :WOhrcst, :WOhcstÑ;                             
                                                                                                    
          YYwSW = *Off;                                                                             
          Dow  Sqlcod= *Zeros;                                                                      
           WIndEfectivi=0;                                                                          
            IF WOhrcst = 'S';                                                                       
                 CampaAux = WOhmeda;                                                                
                 Pricampa = %Trim(WOhmeda);                                                         
                 If WIndEfectivi=0;                                                                 
                  PgmEfectividad(ICia:WOhcstÑ:WOhordÑ:W_Efetividad);                                
                  If W_EfetividadP = ' ' And W_Efetividad = 'S';                                    
                   W_EfetividadP = W_Efetividad;                                                    
                  Endif;                                                                            
                  WIndEfectivi=1;                                                                   
                  IdxN += 1;                                                                        
                 Endif;                                                                             
            EndIF;                                                                                  
            IF WOhrcst = 'C';                                                                       
            Qnro = PrInsuficienciaCte();                                                            
             If Qnro > 0;                                                                           
              YYwSW = *On;                                                                          
              Pricampa = %Trim(WOhmeda);                                                            
                 CampaAux = WOhmeda;                                                                
                 W_Efetividad = 'S';                                                                
                 If W_EfetividadP = ' ' And W_Efetividad = 'S';                                     
                  W_EfetividadP = W_Efetividad;                                                     
                 Endif;                                                                             
                 IdxN += 1;                                                                         
             EndIf;                                                                                 
            EndIF;                                                                                  
           Exec Sql                                                                                 
           Fetch Cursor2mp Into :WOhmeda, :WOhordÑ, :WOhrcst, :WOhcstÑ;                             
          EndDo;                                                                                    
                                                                                                    
          If YYwSW = *On And IdxN >= 2;                                                             
           W_Efetividad = 'S';                                                                      
           W_EfetividadP = W_Efetividad;                                                            
          Endif;                                                                                    
                                                                                                    
          If W_Efetividad = 'S' And IdxN >= Count;                                                  
           Xreg = *blanks;                                                                          
           Xreg = IRegion;                                                                          
           PrEscribeRentable();                                                                     
          Else;                                                                                     
           WApli = 'P';                                                                             
           PrGrabar();                                                                              
          EndIf;                                                                                    
                                                                                                    
           Exec Sql                                                                                 
            Close Cursor2mp;                                                                        
                                                                                                    
      /End-Free                                                                                     
     PPrBusOrdenMpdos  E                                                                            
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento Calcula el Valor Facturado del Primer Pedido       /                         
      //------------------------------------------------------------------/                         
     PPrCalVlrPrimPed  B                                                                            
     DPrCalVlrPrimPed  Pi                                                                           
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       Clear DsVlrTtlPlan;                                                                          
       SqlStr = 'Select Lmgmed , Lmgtvp ' +                                                         
                'From VotreA00/Vtglmgf ' +                                                          
          //    'Where Lmgmed between ' + $Coma + %Trim(w_campanhaTmp) + $Coma +                    
                'Where Lmgmed between ' + $Coma + %Trim(XCampaÑ) + $Coma +                          
                ' And ' +  $Coma + %Trim(ICamPa) + $Coma + ' And ' +                                
                'Lmgzon = ' + $Coma + %Trim(wlmgzon) + $Coma + ' And ' +                            
                'Lmgsec = ' + $Coma + %Trim(wlmgsec) + $Coma + ' And ' +                            
                'Lmgcod = ' + %Trim(%Char(wlmgcod)) + ' Order by Lmgmed Asc';                       
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur2q From :SqlStr;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor2q Cursor For Cur2q;                                                          
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor2q;                                                                              
                                                                                                    
       Exec Sql                                                                                     
        Fetch From Cursor2q For :Elements Rows Into :DsVlrTtlPlan;                                  
       Clear W_NroPeds;                                                                             
       W_NroPeds = Sqler3;                                                                          
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor2q;                                                                             
                                                                                                    
      /End-Free                                                                                     
     PPrCalVlrPrimPed  E                                                                            
                                                                                                    
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento Calcula el Valor Facturado del Primer Pedido       /                         
      //------------------------------------------------------------------/                         
     PPrGrabar         B                                                                            
     DPrGrabar         Pi                                                                           
      *Definición de variables de trabajo                                                           
     DWCedreempla      s              9  0  Inz(*Zeros)                                             
     DWCedAux          s              9  0  Inz(*Zeros)                                             
      /Free                                                                                         
            // Calcula en  Valor a Pagar Inscripciones Efectivas Guías                              
               Yreg = ORegion;                                                                      
               Ysec = WGldsct;                                                                      
              CallP(e) PgmConsvalor(ICia:Yreg:IZona:Ysec:Wlmgmed:OValor);                           
                                                                                                    
       Exec Sql                                                                                     
        Select Rsgcgr Into :WCedreempla                                                             
        From Votrea00/Vtmrsgf                                                                       
        Where Rsgcia = :ICia And (Rsgtpo = ' ' Or Rsgtpo = 'P') And                                 
              Rsgcga = :WGldcte And Rsgzng = :IZona And Rsgscg = :WGldsct                           
              And :PCamRa1 BetWeen Rsgcpi And Rsgcpf                                                
        Order By Rsgcpi Asc                                                                         
        Fetch First 1 Row Only;                                                                     
                                                                                                    
       If WCedreempla > *Zeros;                                                                     
        WCedAux = WGldcte;                                                                          
        WGldcte = WCedreempla;                                                                      
       Endif;                                                                                       
                                                                                                    
        Clear w_iegino;                                                                             
        Clear *Nokey Regieg;                                                                        
        w_iegino='1';                                                                               
        Chain (ICia:ORegion:IZona:WGldsct:WGldcte:WLmgcod:WLmgzon:Wlmgsec                           
               :PCamRa1:w_iegino) Vtwiegf;                                                          
         If Not %Found(Vtwiegf);                                                                    
               IEGino  = w_iegino;                                                                  
               IEGCIA  = ICia;                                                                      
               IEGREG  = ORegion;                                                                   
               IEGZNG  = IZona;                                                                     
               IEGSCG  = WGldsct;                                                                   
               IEGCDG  = WGldcte;                                                                   
               IEGCDC  = WLmgcod;                                                                   
               IEGZNC  = WLmgzon;                                                                   
               If Wlmgsec <> Wsect;                                                                 
                IEGSCC  = Wsect;                                                                    
               Else;                                                                                
                IEGSCC  = Wlmgsec;                                                                  
               Endif;                                                                               
               IEGCMP  = PCamRa1;                                                                   
               IEGITP  = WApli;                                                                     
               IEGVPG  = Ovalor;                                                                    
               IEGTPA  = 'I';                                                                       
              If WLmgime=*Blanks;                                                                   
               IEGTPD  = 'F';                                                                       
              Else;                                                                                 
               IEGTPD  = 'I';                                                                       
              EndIF;                                                                                
               IEGFEC  = %Char(%Date());                                                            
               IEGHOR  = %Char(%Time());                                                            
               IEGUSR  = %Trim(W_USer);                                                             
             Write(E) Regieg;                                                                       
          Else;                                                                                     
               IEGSCG  = WGldsct;                                                                   
               IEGCDG  = WGldcte;                                                                   
               IEGCDC  = WLmgcod;                                                                   
               IEGZNC  = WLmgzon;                                                                   
               IEGSCC  = Wlmgsec;                                                                   
               IEGCMP  = PCamRa1;                                                                   
               IEGITP  = WApli;                                                                     
               IEGVPG  = Ovalor;                                                                    
               IF IEGTPA <> 'S';                                                                    
                IEGTPA  = 'I';                                                                      
               ENDIF;                                                                               
              If WLmgime=*Blanks;                                                                   
               IEGTPD  = 'F';                                                                       
              Else;                                                                                 
               IEGTPD  = 'I';                                                                       
              EndIF;                                                                                
               IEGFEC  = %Char(%Date());                                                            
               IEGHOR  = %Char(%Time());                                                            
               IEGUSR  = %Trim(W_USer);                                                             
             UpDate  Regieg;                                                                        
        Endif;                                                                                      
                                                                                                    
       If WCedreempla > *Zeros;                                                                     
        WCedreempla = *Zeros;                                                                       
        WGldcte = WCedAux;                                                                          
        WCedAux = *Zeros;                                                                           
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrGrabar         E                                                                            
                                                                                                    
      //-------------------------------------------------------------------/                        
      // Procedimiento Valida si el Pedido fue Cancelado por Insuficiencia /                        
      //                         de Mercancía                              /                        
      //-------------------------------------------------------------------/                        
     PPrInsuficienciaCte...                                                                         
     P                 B                                                                            
     DPrInsuficienciaCte...                                                                         
     D                 Pi             9  0                                                          
      ** Definicion de Variables a Utilizar                                                         
     DQnro             s              9  0 Inz(*Zeros)                                              
      /Free                                                                                         
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Select Count(Distinct(OdordÑ)) ' +                                                
                 'From Opf' + ICia + '/Pordtl ' +                                                   
                 'Where Odscus = ' + %Trim(%Editc(WLmgcod:'Z')) +                                   
                 ' And OdordÑ = ' + %Trim(%Editc(WOhordÑ:'Z')) +                                    
             ' And (Odlccd = ''INS''' + ' Or Odlccd = ''BAC''' + ' Or ' +                           
             'Odlccd = ''CCD''' + ')';                                                              
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1q From :SqlStr;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1q Cursor For Cur1q;                                                          
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1q;                                                                              
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1q Into :Qnro;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1q;                                                                             
                                                                                                    
       Return Qnro;                                                                                 
      /End-Free                                                                                     
     PPrInsuficienciaCte...                                                                         
     P                 E                                                                            
                                                                                                    
      **Free                                                                                        
        ////////////////////////////////////////////////////////////////////////                    
        // Prototipo: PrEscribeRentable                                                             
        //                                                                                          
        // * Debe permitir seleccionar varios pedidos consecutivos, si bien actualmente son dos,    
        // puede presentarse cambios a futuro.  (no es lo ideal, pero puede presentarse)            
        // * Debe permitir elegir si el monto es acumulado entre todos los pedidos ejemplo, si el   
        // monto de la inscripción rentable es por dos pedidos consecutivos por un valor acumulado  
        // de $100.000 este monto se puede hacer 1er pedido por $70.000 y el segundo pedido por     
        // $30.000 cumpliendo de esta manera el valor rentable indicado.                            
        // * O monto individual ejemplo si son dos pedidos, debe hacer 1er pedido minino por        
        // $100.000 y el 2do pedido consecutivo minino  por $110.000                                
        ////////////////////////////////////////////////////////////////////////                    
       Dcl-Proc PrEscribeRentable;                                                                  
        Dcl-s w_cantCondi Zoned(3);                                                                 
        Dcl-s w_PCamE Like(PCamE);                                                                  
        Dcl-s w_PCamR Like(PCamR);                                                                  
        Dcl-s w_SwtAcum Ind;                                                                        
        Dcl-s w_SwtIndi Ind;                                                                        
        Dcl-s w_AcumuladoParm Packed(11:2);                                                         
        Dcl-s w_AcumuladoPedi Packed(11:2);                                                         
        Dcl-s w_Registro Char(2000);                                                                
                                                                                                    
        Clear w_Registro;                                                                           
        Clear w_AcumuladoParm;                                                                      
        Clear w_AcumuladoPedi;                                                                      
        w_SwtIndi = *Off;                                                                           
        w_SwtAcum = *Off;                                                                           
        Clear w_cantCondi;                                                                          
        Clear DsValrRenta;                                                                          
        Clear ISector;                                                                              
        If Wlmgsec <> Wsect;                                                                        
           ISector = Wsect;                                                                         
        Else;                                                                                       
           ISector = Wlmgsec;                                                                       
        Endif;                                                                                      
        //Recupera Valor parametrizado para inscripción rentable                                    
        //Parámetros: Compa¦ia:Región:Zona:Salida Cantidad:Estructura valor Rentable                
        CallP(e) PgmVlrInsRent(ICia:Xreg:IZona:ISector:w_cantCondi:DsValrRenta);                    
        Clear w_PCamR;                                                                              
        If w_cantCondi > 1;                                                                         
           w_PCamE = Wlmgmed;                                                                       
           POper = '-';                                                                             
           Pnume = %Char(w_cantCondi-1);                                                            
           If Wlmgome = '2010' And ICamPa = '2010' And %Trim(Pnume) = '1';                          
            Pnume = '2';                                                                            
           Endif;                                                                                   
           callp(E) ProCampa(Icia:w_PCamE:w_PCamR:POper:Pnume);                                     
           Clear w_campanhaTmp;                                                                     
           w_campanhaTmp = w_PCamR;                                                                 
        Else;                                                                                       
           w_campanhaTmp = Wlmgmed;                                                                 
        EndIf;                                                                                      
        XCampaÑ=Wlmgmed;                                                                            
        // Calcula Valor pedido(s) consecutivo(s)                                                   
        PrCalVlrPrimPed();                                                                          
        w_REGISTRO = 'código:'+%trim(%char(wlmgcod))+'cant parm:'+                                  
        %trim(%char(w_cantCondi))+'Nr ped conse:' +%Trim(%char(W_NroPeds))+                         
        'PGM : VTPSGBBR '+                                                                          
        'Cod Monto:' + %Trim(%Char(DsValrRenta(1).OcodMod));                                        
        Exec Sql                                                                                    
        Insert Into GNPNALSOF4.PRUEBALE (REGISTRO) Values(:w_REGISTRO);                             
        // verifica condición para Monto Individual                                                 
       // If DsValrRenta(1).OcodMod = 1 And W_NroPeds = w_cantCondi;                                
        If DsValrRenta(1).OcodMod = 1;                                                              
           For Dx1 = 1 to w_cantCondi by 1;                                                         
               // verifica condición para Monto Individual                                          
               If DsVlrTtlPlan(Dx1).dsVlr <= DsValrRenta(Dx1).OVlrPed                               
                  and w_SwtIndi = *On;                                                              
                  w_SwtIndi = *On;                                                                  
                  Leave;                                                                            
               EndIf;                                                                               
           EndFor;                                                                                  
        // verifica condición para Monto Acumulado                                                  
        ElseIf DsValrRenta(1).OcodMod = 2;                                                          
           For dx1 = 1 to w_cantCondi by 1;                                                         
              w_AcumuladoParm += DsValrRenta(Dx1).OVlrPed;                                          
           EndFor;                                                                                  
           For dx1 = 1 to W_NroPeds by 1;                                                           
              w_AcumuladoPedi += DsVlrTtlPlan(Dx1).dsVlr;                                           
           EndFor;                                                                                  
           // verifica condición para Monto Acumulado                                               
           If w_AcumuladoPedi - w_AcumuladoParm < *Zeros and w_AcumuladoPedi>0;                     
              w_SwtAcum = *On;                                                                      
           EndIf;                                                                                   
        EndIf;                                                                                      
        //Si cumple condición para pedido individual o acumulado, es marcada en 'E'                 
        If (W_SwtIndi = *Off  And DsValrRenta(1).OcodMod = 1 And                                    
         // W_NroPeds >*Zeros And W_NroPeds = w_cantCondi)         Or                               
            W_NroPeds >*Zeros) Or                                                                   
           (w_AcumuladoPedi > *Zeros And w_SwtAcum = *Off    And                                    
            DsValrRenta(1).OcodMod = 2);                                                            
           WApli = 'E';                                                                             
           PrGrabar();                                                                              
        Else;                                                                                       
           Ovalor = *Zeros;                                                                         
           WApli = 'N';                                                                             
           PrGrabar();                                                                              
        EndIf;                                                                                      
       End-Proc;                                                                                    
