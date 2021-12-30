      **********************************************************************                        
      ** Programa     : VTPSGB10R                                         **                        
      ** Descripción: : Pg Inscripciones  Re-Incorporaciones Escalonadas  **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Creación:  28 de Abril 2019                                **                        
      ** IEGINO (Indicador de origen)=  '2' incorporaciones escalonadas   **                        
      **********************************************************************                        
     HOption(*NoDebugIo)                                                                            
     HDFTACTGRP(*NO)                                                                                
                                                                                                    
     ** Definición de Archivo de Trabajo                                                            
                                                                                                    
       Dcl-F Vtwiegf Usage(*INPUT:*OUTPUT:*UPDATE:*DELETE) Keyed;                                   
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
       Dcl-ds Fields Extname('VTWIEGF') Prefix(Q) End-ds;                                           
                                                                                                    
       Dcl-Pr Main Extpgm('Main');                                                                  
         Icia Char(3);                                                                              
         ICamPa Char(4);                                                                            
         IZona Char(3);                                                                             
       End-Pr;                                                                                      
                                                                                                    
       Dcl-PI Main;                                                                                 
         Icia Char(3);                                                                              
         ICamPa Char(4);                                                                            
         IZona Char(3);                                                                             
       End-PI;                                                                                      
     ** Definición de Estructuras de Trabajo                                                        
                                                                                                    
      * Definición de Vectores                                                                      
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
      * Estructura recupera valor parametrizado para inscripciones escalonadas                      
       Dcl-Ds DsValrInsEscalo Dim(999) Qualified;                                                   
         Ocondicion Packed(4);                                                                      
         OrangoIni  Packed(4);                                                                      
         OrangoFin  Packed(4);                                                                      
         OperadorLo Char(2);                                                                        
         OvlrInsrRenta Packed(11:2);                                                                
       End-Ds;                                                                                      
      *Definición de variables de trabajo                                                           
         Dcl-s PCamRa      Char(4) Inz(*Blanks);                                                    
         Dcl-s PCamRa1     Char(4) Inz(*Blanks);                                                    
         Dcl-s WApli       Char(1) Inz(*Blanks);                                                    
         Dcl-s cia         Char(3) Inz(*Blanks);                                                    
         Dcl-s Xcia        Char(3) Inz(*Blanks);                                                    
         Dcl-s Pres        Char(2) Inz(*Blanks);                                                    
         Dcl-s PCia        Char(3)   Inz(*Blanks);                                                  
         Dcl-s PCamE       Char(4) Inz(*Blanks);                                                    
         Dcl-s PCamR       Char(4) Inz(*Blanks);                                                    
         Dcl-s POper       Char(1) Inz(*Blanks);                                                    
         Dcl-s Pnume       Char(3)   Inz(*Blanks);                                                  
         Dcl-s User        Char(10)   Inz(*User);                                                   
         Dcl-s W_I         Char(1) Inz(*Blanks);                                                    
         Dcl-s W_R         Char(1) Inz(*Blanks);                                                    
         Dcl-s Xced        Char(10)   Inz(*Blanks);                                                 
         Dcl-s Xplan       Char(10)   Inz(*Blanks);                                                 
         Dcl-s Isector     Char(3)   Inz(*Blanks);                                                  
         Dcl-s WOhrcst     Char(1) Inz(*Blanks);                                                    
         Dcl-s ICedula     Packed(15) Inz(*Zeros);                                                  
         Dcl-s OREGION     Char(10)   Inz(*Blanks);                                                 
         Dcl-s OValor      Packed(11:2) Inz(*Zeros);                                                
         Dcl-s Xreg        Char(30)   Inz(*Blanks);                                                 
         Dcl-s WValor      Packed(12:2) Inz(*Zeros);                                                
         Dcl-s W_USER      Char(10)   Inz(*User);                                                   
         Dcl-s Icaso       Char(7)   Inz(*Blanks);                                                  
         Dcl-s Ifecha      Char(10)   Inz(*Blanks);                                                 
         Dcl-s IcedulaX    Char(30)   Inz(*Blanks);                                                 
         Dcl-s Orespuesta  Char(1) Inz(*Blanks);                                                    
         Dcl-s Wsect       Char(1) Inz(*Blanks);                                                    
         Dcl-s PciaV       Char(3) Inz('999');                                                      
         Dcl-s Xrol        Char(2) Inz('CM');                                                       
         Dcl-s Xfec        Char(10) Inz(*Blanks);                                                   
         Dcl-s Xtexto      Char(200) Inz(*Blanks);                                                  
         Dcl-s W_NroInsc   Zoned(3);                                                                
         Dcl-s dx1         Zoned(4);                                                                
         Dcl-s dx2         Zoned(4);                                                                
         Dcl-s WvalorPagar Packed (11:2);                                                           
         Dcl-s w_Registro  Char(2000);                                                              
            // Indicador de origen----Reincorporaciones escalonadas                                 
         Dcl-s w_Iegino    Char(2) Inz(*Blanks);                                                    
         Dcl-s w_CntRango  Zoned(3);                                                                
         Dcl-s w_SwtIndi   Ind;                                                                     
         Dcl-s WTsec       Char(1) Inz(*Blanks);                                                    
                                                                                                    
         Dcl-s Ocant       Packed(9);                                                               
         Dcl-s Icamins     Char(4);                                                                 
         Dcl-s Iinstot     Packed(5);                                                               
         Dcl-s Iinsdel     Packed(9);                                                               
         Dcl-s IFoco       Char(30);                                                                
         Dcl-s Inombreg    Char(30);                                                                
         Dcl-s Icedulag    Packed(9);                                                               
      *Definición de Procedimientos                                                                 
       Dcl-Pr ProCampa  Extpgm('VOTREP00/VTMCSBKR');                                                
        PCia   Char(3);                                                                             
        PCamE  Char(4);                                                                             
        PCamR  Char(4);                                                                             
        POper  Char(1);                                                                             
        Pnume  Char(3);                                                                             
       End-Pr;                                                                                      
                                                                                                    
      * Programa validar region                                                                     
       Dcl-Pr Pgmregio  Extpgm('VOTREP00/VTMGOIGR');                                                
        PCia      Char(3);                                                                          
        Izona     Char(3);                                                                          
        ORegion   Char(10);                                                                         
        Oced      Char(10);                                                                         
        Oplan     Char(10);                                                                         
       End-Pr;                                                                                      
                                                                                                    
                                                                                                    
      * Programa valida las N Campa¦as                                                              
       Dcl-Pr PgmNCampa  Extpgm('VOTREP00/VTMPGBBR');                                               
        PCia      Char(3);                                                                          
        IRegion   Char(10);                                                                         
        Izona     Char(3);                                                                          
        Isector   Char(3);                                                                          
        ICamPa    Char(4);                                                                          
        Onro      Packed(4);                                                                        
       End-Pr;                                                                                      
                                                                                                    
     ** Consulta efectividad de Pedido                                                              
       Dcl-Pr PgmEfectividad  Extpgm('VOTREP00/VTPEFE0R');                                          
         XCia  Char(3);                                                                             
         XCed  Packed(9);                                                                           
         XOrd  Packed(9);                                                                           
         XEfe  Char(3);                                                                             
       End-pr;                                                                                      
                                                                                                    
      * programa Valor a Pagar Inscripciones Efectivas Guías                                        
       Dcl-Pr PgmConsvalor    Extpgm('VOTREP00/VTMPGBAR');                                          
        Ycia   Char(3);                                                                             
        Yreg   Char(30);                                                                            
        Yzon   Char(3);                                                                             
        Ysec   Char(3);                                                                             
        Ycam   Char(4);                                                                             
        Yoval  Packed(11:2);                                                                        
       End-Pr;                                                                                      
                                                                                                    
      * programa valida Valor facturado                                                             
       Dcl-Pr PgmVlrInsEsca  Extpgm('VOTREP00/VTMPGBWR');                                           
        Xcia     Char(3);                                                                           
        Xreg     Char(30);                                                                          
        Xzon     Char(3);                                                                           
        Xsec     Char(3);                                                                           
        XNroRang Zoned(3);                                                                          
        Xoval LikeDs(DsValrInsEscalo) Dim(999);                                                     
       End-Pr;                                                                                      
         Dcl-Pr PgmNewPlanG Extpgm('VOTREP00/VTMPGB0R');                                            
            Pcia        Char(3);                                                                    
            Izona       Char(3);                                                                    
            Icaso       Char(7);                                                                    
            IcedulaX    Char(30);                                                                   
            Ifecha      Char(10);                                                                   
            Orespuesta  Char(1);                                                                    
         End-Pr;                                                                                    
         // Recupera Cantidad de inscripciones efectivas                                            
         Dcl-Pr PgmCantInscrip Extpgm('VOTREP00/VTPSGB11R');                                        
            Icia        Char(3);                                                                    
            Icamp       Char(4);                                                                    
            Izona       Char(3);                                                                    
            Isector     Char(1);                                                                    
            Icedulag    Packed(9);                                                                  
            Inombreg    Char(30);                                                                   
            IFoco       Char(30);                                                                   
            Iinstot     Packed(5);                                                                  
            Iinsdel     Packed(9);                                                                  
            Icamins     Char(4);                                                                    
            Ocant       Packed(9);                                                                  
         End-Pr;                                                                                    
         Dcl-Pr PgmMigracion Extpgm('VOTREP00/VTPLDITR');                                           
         Xcia Char(3);                                                                              
         Xrol Char(2);                                                                              
         Xfec Char(10);                                                                             
         Xtexto Char(200);                                                                          
         End-Pr;                                                                                    
         Dcl-Ds DS_PEDIDOS;                                                                         
          DS_CODG  Packed(9);                                                                       
          DS_SECT  Char(1);                                                                         
          DS_CODC  Zoned(9);                                                                        
         End-Ds;                                                                                    
         Dcl-Ds DS_fields6 LIKEDS(DS_PEDIDOS) DIM(10000);                                           
         Dcl-S Elements ZONED(5:0) Inz(%Elem(DS_fields6));                                          
         Dcl-s W_CantGuias Zoned(9);                                                                
         //DCL-DS DS_VTWIEGF EXTNAME('VTWIEGF') QUALIFIED END-DS;                                   
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
                                                                                                    
       Clear WTsec;                                                                                 
       //Resta de la  Campa¦a Cierre de Zona Menos 5 Campa¦as                                       
       PCamE = ICamPa;                                                                              
       PCamR = *Blanks;                                                                             
       POper = '-';                                                                                 
       Pnume = '1';                                                                                 
       If ICampa = '2010';                                                                          
        Pnume = '002';                                                                              
       Endif;                                                                                       
       callp(E) ProCampa(Icia:PCamE:PCamR:POper:Pnume);                                             
       PCamRa1 = *Blanks;                                                                           
       PCamRa1 = PCamR;                                                                             
                                                                                                    
       // Se valida con el Stencil de la Campa¦a de Cierre las                                      
       // Inscripciones o Reincorporaciones de la Campa¦a Anterior                                  
       // Sin Evaluar la Zona y el Sector                                                           
       Exec Sql                                                                                     
       Declare Cur01 Cursor for                                                                     
       Select gldcte, gldsct , IEGCDC                                                               
       From (Select distinct gldcte, gldsct ,                                                       
       Ifnull(DEC((Select IEGCDC                                                                    
            From votrea00.Vtwiegf a                                                                 
            Where IEGCIA=gldcia And IEGZNG= gldzon                                                  
            And IEGCDG= gldcte  And iegcmp=Trim(:PCamRa1)                                           
            And Trim(IegIno) = '1' And Trim(IegItp) = 'E'                                           
            order by rrn(a) asc                                                                     
            FETCH FIRST 1 ROWS ONLY), 9 ,  0 ) , 0) AS IEGCDC                                       
       From Votrea00.Vtmgldf                                                                        
       Where gldcia = Trim(:ICia)                                                                   
         And Gldsct <> ' '                                                                          
         And gldzon = Trim(:Izona)) AS A                                                            
          Where IEGCDC>0 order by gldcte asc , IEGCDC asc;                                          
       Exec sql                                                                                     
       Open Cur01;                                                                                  
                                                                                                    
       Exec sql                                                                                     
       Fetch From Cur01 For :Elements Rows Into :DS_fields6;                                        
       W_CantGuias = Sqler3;                                                                        
       Exec sql                                                                                     
       Close Cur01;                                                                                 
       For dx2 = 1 to W_CantGuias by 1;                                                             
         QIEGCDG = DS_fields6(dx2).DS_CODG;                                                         
         QIEGCDC = DS_fields6(dx2).DS_CODC;                                                         
         QIEGSCG = DS_fields6(dx2).DS_SECT;                                                         
         QIEGSCC = DS_fields6(dx2).DS_SECT;                                                         
         QIEGZNC = Izona;                                                                           
         QIEGCMP = PCamRa1;                                                                         
         PrEscribeRentable();                                                                       
       EndFor;                                                                                      
                                                                                                    
        *Inlr = *On;                                                                                
                                                                                                    
      //------------------------------------------------------------------/                         
      // Procedimiento Calcula el Valor Facturado del Primer Pedido       /                         
      //------------------------------------------------------------------/                         
       Dcl-Proc PrGrabar;                                                                           
      *Definición de variables de trabajo                                                           
         Dcl-s WCedreempla Zoned(9) Inz(*zeros);                                                    
         Dcl-s WCedAux Zoned(9) Inz(*zeros);                                                        
         Dcl-s Tnro Zoned(5) Inz(*zeros);                                                           
         Clear WCedAux;                                                                             
         Clear WCedReempla;                                                                         
         Exec Sql                                                                                   
         Select Rsgcgr Into :WCedreempla                                                            
         From Votrea00.Vtmrsgf                                                                      
         Where Rsgcia = :ICia And (Rsgtpo = ' ' Or Rsgtpo = 'P') And                                
               Rsgcga = :QIEGCDG And Rsgzng = :IZona And Rsgscg = :QIEGSCG                          
               And :QIEGCMP BetWeen Rsgcpi And Rsgcpf                                               
         Order By Rsgcpi Asc                                                                        
         Fetch First 1 Row Only;                                                                    
                                                                                                    
         If WCedreempla > *Zeros;                                                                   
            WCedAux = WGldcte;                                                                      
            WGldcte = WCedreempla;                                                                  
         Endif;                                                                                     
                                                                                                    
         Clear Tnro;                                                                                
         Exec Sql                                                                                   
          Select Count(0) Into :Tnro                                                                
          From Votrea00/Vtwiegf                                                                     
          Where Iegcia = :ICia And                                                                  
                Iegzng = :IZona And                                                                 
                Iegscg = :QIEGSCG And                                                               
                Iegcdg = :QIEGCDG And                                                               
                Iegcmp = :QIEGCMP And                                                               
                Iegvpg <> 0;                                                                        
                                                                                                    
         Clear w_Iegino;                                                                            
         w_Iegino='2';                                                                              
         Chain (ICia:QIEGREG:IZona:QIEGSCG:QIEGCDG:QIEGCDC:QIEGZNC:QIEGSCC                          
               :QIEGCMP:w_Iegino) Vtwiegf;                                                          
         If Not %Found(Vtwiegf);                                                                    
            IEGINO  = w_Iegino;                                                                     
            IEGCIA  = ICia;                                                                         
            IEGREG  = QIEGREG;                                                                      
            IEGZNG  = IZona;                                                                        
            IEGSCG  = QIEGSCG;                                                                      
            IEGCDG  = QIEGCDG;                                                                      
            IEGCDC  = QIEGCDC;                                                                      
            IEGZNC  = QIEGZNC;                                                                      
            IEGSCC  = QIEGSCC;                                                                      
            IEGCMP  = QIEGCMP;                                                                      
            IEGITP  = WApli;                                                                        
            If Tnro = 0;                                                                            
             IEGVPG  = Ovalor;                                                                      
            Endif;                                                                                  
            IEGTPA  = 'I';                                                                          
            PrInsufiMercancia();                                                                    
            If QIEGTPD = *Blanks;                                                                   
               IEGTPD  = 'F';                                                                       
            Else;                                                                                   
               IEGTPD  = 'I';                                                                       
            EndIF;                                                                                  
            IEGFEC  = %Char(%Date());                                                               
            IEGHOR  = %Char(%Time());                                                               
            IEGUSR  = %Trim(W_USer);                                                                
            Write(E) Regieg;                                                                        
         Else;                                                                                      
            IEGSCG  = QIEGSCG;                                                                      
            IEGCDG  = QIEGCDG;                                                                      
            IEGCDC  = QIEGCDC;                                                                      
            IEGZNC  = QIEGZNC;                                                                      
            IEGSCC  = QIEGSCC;                                                                      
            IEGCMP  = QIEGCMP;                                                                      
            IEGITP  = WApli;                                                                        
         // IEGVPG  = Ovalor;                                                                       
            IF IEGTPA <> 'S';                                                                       
             IEGTPA  = 'I';                                                                         
             IEGVPG  = Ovalor;                                                                      
            ENDIF;                                                                                  
            PrInsufiMercancia();                                                                    
            If QIEGTPD = *Blanks;                                                                   
               IEGTPD  = 'F';                                                                       
            Else;                                                                                   
               IEGTPD  = 'I';                                                                       
            EndIF;                                                                                  
            IEGFEC  = %Char(%Date());                                                               
            IEGHOR  = %Char(%Time());                                                               
            IEGUSR  = %Trim(W_USer);                                                                
            UpDate(E)  Regieg;                                                                      
         Endif;                                                                                     
         w_REGISTRO = 'código guía: '+%trim(%char(QIEGCDG))+' cant '+                               
         'inscripParm: '+%trim(%char(w_CntRango))+' Nr InscrIpcionesGuia: '+                        
         %Trim(%Char(W_NroInsc)) + ' PGM: VTPSGB10R Valor pagado: '+                                
         %Char(Ovalor)+ 'Estado orden:'+WOhrcst +'Aplica:'+WApli+                                   
         ' Fecha' +%Char(%Date());                                                                  
                                                                                                    
         If W_SwtIndi = *On;                                                                        
            w_REGISTRO = %Trim(w_REGISTRO) +                                                        
            ' RangIni: ' + %Char(DsValrInsEscalo(Dx1).OrangoIni) +                                  
            'RangFin: ' + %Char(DsValrInsEscalo(Dx1).OrangoFin);                                    
         Else;                                                                                      
            w_REGISTRO = %Trim(w_REGISTRO) +                                                        
            ' RangIni: no aplico codicion RangFin: no aplico codicion';                             
         EndIf;                                                                                     
         Exec Sql                                                                                   
         Insert Into GNPNALSOF4.PRUEBALE (REGISTRO) Values(:w_REGISTRO);                            
                                                                                                    
         If WCedreempla > *Zeros;                                                                   
            WCedreempla = *Zeros;                                                                   
            WGldcte = WCedAux;                                                                      
            WCedAux = *Zeros;                                                                       
         Endif;                                                                                     
       End-Proc;                                                                                    
                                                                                                    
        ////////////////////////////////////////////////////////////////////////////                
        // Prototipo: PrEscribeRentable                                           //                
        // si en la tabla se tienen 3 rangos como seria                           //                
        // 1er rango por 1 inscripciones rentables se paga 2.000                  //                
        // 2do rango de 2 a 4 inscripciones rentables se paga 5.000               //                
        // 3er rango igual o mayor a 5  inscripciones rentables se paga 5.000     //                
        //                                                                        //                
        // Si una líder al cierre de la campaÂ¦a tuvo 4 inscripciones rentables,   //               
        // esta se ubica en el 2do rango, donde por cada inscripción se paga 5000,//                
        // en total gana 4 * 5000 = 20.000 quiere decir que por inscripciones     //                
        //                                                                        //                
        // Si la líder tuvo 7 inscripciones rentables, se ubica en el ultimo      //                
        // rango y ganaría por este concepto  7 * 5000 = 35.000                   //                
        //                                                                        //                
        ////////////////////////////////////////////////////////////////////////////                
       Dcl-Proc PrEscribeRentable;                                                                  
         Dcl-s w_PCamE Like(PCamE);                                                                 
         Dcl-s w_PCamR Like(PCamR);                                                                 
         Dcl-s w_SwtAcum Ind;                                                                       
         Dcl-s w_AcumuladoParm Packed(11:2);                                                        
         Dcl-s w_AcumuladoPedi Packed(11:2);                                                        
                                                                                                    
         Clear w_Registro;                                                                          
         Clear w_AcumuladoParm;                                                                     
         Clear w_AcumuladoPedi;                                                                     
         w_SwtIndi = *Off;                                                                          
         w_SwtAcum = *Off;                                                                          
         Clear w_CntRango;                                                                          
         Clear DsValrInsEscalo;                                                                     
         Clear Xreg;                                                                                
         CallP(e) Pgmregio(ICia:IZona:ORegion:Xced:xplan);                                          
         QIEGREG =  ORegion;                                                                        
         Xreg = QIEGREG;                                                                            
         //Recupera la cantidad de inscripciones efectivas por campaña                              
         Icedulag=QIEGCDG;                                                                          
         PgmCantInscrip(ICia:PCamRa1:IZona:QIEGSCG:Icedulag:Inombreg:Ifoco                          
         :Iinstot :Iinsdel:Icamins:OCant);                                                          
         W_NroInsc = OCant;                                                                         
         //Recupera Valor parametrizado para inscripción rentable                                   
         //Parámetros: CompaÂ¦ia:Región:Zona:Salida Cantidad:Estructura valor Rentable              
         ISector=QIEGSCG;                                                                           
         CallP(e) PgmVlrInsEsca(ICia:Xreg:IZona:ISector:w_CntRango                                  
                                 :DsValrInsEscalo);                                                 
         W_SwtIndi=*Off;                                                                            
         Clear WvalorPagar;                                                                         
         If w_CntRango > *Zeros and W_NroInsc > *Zeros;                                             
            //Contador descendete debido a que el consumo de inscripciones                          
            //se llega ordenado ASC                                                                 
            For Dx1 = w_CntRango by 1 downto 1;                                                     
               // verifica condición para cantidad de inscripción                                   
               IF Dx1 = w_CntRango;                                                                 
                  IF %Trim(DsValrInsEscalo(Dx1).OperadorLo) = '>'                                   
                  And W_NroInsc > DsValrInsEscalo(Dx1).OrangoIni;                                   
                     WvalorPagar = DsValrInsEscalo(Dx1).OvlrInsrRenta*W_NroInsc;                    
                     W_SwtIndi = *On;                                                               
                     Leave;                                                                         
                  ElseIF %Trim(DsValrInsEscalo(Dx1).OperadorLo) = '>='                              
                  And W_NroInsc >= DsValrInsEscalo(Dx1).OrangoIni;                                  
                     WvalorPagar = DsValrInsEscalo(Dx1).OvlrInsrRenta*W_NroInsc;                    
                     W_SwtIndi = *On;                                                               
                     Leave;                                                                         
                  ElseIF %Trim(DsValrInsEscalo(Dx1).OperadorLo) = '='                               
                  And W_NroInsc = DsValrInsEscalo(Dx1).OrangoIni;                                   
                     WvalorPagar = DsValrInsEscalo(Dx1).OvlrInsrRenta*W_NroInsc;                    
                     W_SwtIndi = *On;                                                               
                     Leave;                                                                         
                  EndIF;                                                                            
               ElseIf W_NroInsc >= DsValrInsEscalo(Dx1).OrangoIni                                   
                  and W_NroInsc <= DsValrInsEscalo(Dx1).OrangoFin;                                  
                  WvalorPagar = DsValrInsEscalo(Dx1).OvlrInsrRenta*W_NroInsc;                       
                  W_SwtIndi = *On;                                                                  
                  Leave;                                                                            
               EndIf;                                                                               
            EndFor;                                                                                 
            //Si cumple condición para cantidad de inscripciones por campa¦a                        
            If W_SwtIndi = *On  And WvalorPagar > *Zeros;                                           
               Ovalor = WvalorPagar;                                                                
               WApli = 'E';                                                                         
               PrGrabar();                                                                          
            Else;                                                                                   
               Clear Ovalor;                                                                        
               WApli = 'N';                                                                         
               PrGrabar();                                                                          
            EndIf;                                                                                  
         EndIf;                                                                                     
         Clear Ovalor;                                                                              
       End-Proc;                                                                                    
       Dcl-Proc PrInsufiMercancia;                                                                  
            Clear QIEGTPD;                                                                          
            Exec Sql                                                                                
            Select Distinct Lmgime into :QIEGTPD                                                    
            From votrea00.Vtglmgf                                                                   
            Where Lmgcia =:ICia                                                                     
              and Lmgmed = :QIEGCMP                                                                 
              and Lmgzon = :IZona                                                                   
              And Lmgcod = :QIEGCDC;                                                                
       End-Proc;                                                                                    
                                                                                                    
