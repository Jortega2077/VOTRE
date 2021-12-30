      **********************************************************************                        
      ** Programa     : VTPSGBMR                                          **                        
      ** Descripciòn: : Pg Valida las Politicas Comerciales por Guía y    **                        
      **           Determina si se puede Realizar el Pago o No por Campaña**                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Creaciòn: 14 de Junio de 2016                              **                        
      **********************************************************************                        
     HOption(*NoDebugIo)                                                                            
     HDFTACTGRP(*NO)                                                                                
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
     FVttdngf   UF A E           K DISK                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DIcia                            3a                                                            
     DICamPa                          4A                                                            
     DIZona                           3a                                                            
     DICedulaG                        9  0                                                          
     DISectorG                        3a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DIcia                            3a                                                            
     DICamPa                          4a                                                            
     DIZona                           3a                                                            
     DICedulaG                        9  0                                                          
     DISectorG                        3a                                                            
                                                                                                    
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
     DORegion                        30a                                                            
     DOced                           10A                                                            
     DOplan                          10a                                                            
                                                                                                    
                                                                                                    
      *Definición de Varibles de Trabajo                                                            
     DWsql             s           1000a   Inz(*Blanks)                                             
     DORegion          s             30A   Inz(*Blanks)                                             
     DOCed             s             10A   Inz(*Blanks)                                             
     DOplan            s             10a   Inz(*Blanks)                                             
     D$COMA            C                   ''''                                                     
     DW_USER           s             10a   Inz(*User)                                               
     DW_Cia            s              3a   Inz(*Blanks)                                             
     DW_Zona           s              3a   Inz(*Blanks)                                             
     DW_Camp           s              4a   Inz(*Blanks)                                             
     DWNro             s              4  0 Inz(*Zeros)                                              
     DWDngvlr          s             11  2 Inz(*Zeros)                                              
     DXValor           s             11  2 Inz(*Zeros)                                              
     DW_REG            s             30a   Inz(*Blanks)                                             
     DW_Nro            s              4  0 Inz(*Zeros)                                              
     DWPcgdes          s            200a   Inz(*Blanks)                                             
     DWPcgcdg          s              9  0 Inz(*Zeros)                                              
     DWPcgcdp          s              4  0 Inz(*Zeros)                                              
     DWPcgInd          s              1a   Inz(' ')                                                 
     DW_Motiv          s            500a   Inz(*Blanks)                                             
     DX_MOTIV          s            500a   Inz(*Blanks)                                             
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
                                                                                                    
     DConversion       Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DYcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
                                                                                                    
       PCamE = ICampa;                                                                              
       PCamR = *Blanks;                                                                             
       POper = '-';                                                                                 
       Pnume = '001';                                                                               
       proCampa(Icia:PCamE:PCamR:POper:Pnume);                                                      
          // pgm retorna Región                                                                     
          CallP(E) Pgmregio(ICia:IZona:ORegion:Oced:Oplan);                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select Count(0) ' +                                                                  
              'From VotreA00/VtwPcgf ' +                                                            
              'Where Pcgcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'Pcgcdg = ' + %trim(%char(ICedulaG))           + ' And ' +                            
              'PcgZna = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Pcgsct = ' + $Coma + %trim(ISectorG)  + $Coma + ' And ' +                            
              'Pcgcmp = ' + $Coma + %trim(PCamR)    + $Coma + ' And ' +                             
              'PcgInd = ''N''';                                                                     
       Exec Sql                                                                                     
         Prepare Cursor1 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor1 Scroll Cursor For Cursor1;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor1;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor1 Into :WNro;                                                                  
                                                                                                    
         W_Nro = *Zeros;                                                                            
        If  sqlcod=*Zeros;                                                                          
         W_Nro = WNro;                                                                              
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor1;                                                                             
                                                                                                    
       IF W_Nro > *Zeros;                                                                           
       Wsql = *Blanks;                                                                              
       Wsql = 'Update VotreA00/VtmSpgf Set Spginp = ''E'' ' +                                       
              'Where Spgcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'SpgZon = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Spgcam = ' + $Coma + %trim(PCamR)    + $Coma + ' And ' +                             
              'Spgsec = ' + $Coma + %trim(ISectorG)  + $Coma + ' And ' +                            
              'Spgced = ' + %trim(%char(ICedulaG))           + ' And ' +                            
              'SpgTrr = '' ''';                                                                     
                                                                                                    
        Exec Sql                                                                                    
         Execute Immediate :WSql;                                                                   
       EndIf;                                                                                       
                                                                                                    
       // Consulta por RUT no Enviado                                                               
       PrPagosPendientesGuias();                                                                    
       Wsql = *Blanks;                                                                              
       Clear WNro;                                                                                  
       Wsql = 'Select Count(0) ' +                                                                  
              'From VotreA00/Vttpprf ' +                                                            
              'Where Pprcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'Pprcdg = ' + %trim(%char(ICedulaG))           + ' And ' +                            
              'Pprzna = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Pprsec = ' + $Coma + %trim(ISectorG)  + $Coma + ' And ' +                            
              'Pprcmp = ' + $Coma + %trim(PCamR) + $Coma;                                           
       Exec Sql                                                                                     
         Prepare Cursor1r From :Wsql;                                                               
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor1r Scroll Cursor For Cursor1r;                                               
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor1r;                                                                             
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor1r Into :WNro;                                                                 
                                                                                                    
         W_Nro = *Zeros;                                                                            
        If sqlcod=*Zeros;                                                                           
         W_Nro = WNro;                                                                              
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor1r;                                                                            
                                                                                                    
       IF W_Nro > *Zeros;                                                                           
       Wsql = *Blanks;                                                                              
       Wsql = 'Update VotreA00/VtmSpgf Set Spginp = ''E'' ' +                                       
              'Where Spgcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'SpgZon = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Spgcam = ' + $Coma + %trim(PCamR)    + $Coma + ' And ' +                             
              'Spgsec = ' + $Coma + %trim(ISectorG)  + $Coma + ' And ' +                            
              'Spgced = ' + %trim(%char(ICedulaG))           + ' And ' +                            
              'SpgTrr = '' ''';                                                                     
                                                                                                    
        Exec Sql                                                                                    
         Execute Immediate :WSql;                                                                   
       EndIf;                                                                                       
                                                                                                    
        PrVttdngf();                                                                                
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Graba Información de cada Motivo de NO Pago VTTDNGF //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrVttdngf        B                                                                            
     DPrVttdngf        Pi                                                                           
      /Free                                                                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select Pcgcdg, Pcgdes, Pcgcdp, PcgInd ' +                                            
              'From VotreA00/Vtwpcgf ' +                                                            
              'Where Pcgcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'Pcgcdg = ' + %trim(%char(ICedulaG))           + ' And ' +                            
              'PcgZna = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Pcgsct = ' + $Coma + %trim(ISectorG)  + $Coma + ' And ' +                            
              'Pcgcmp = ' + $Coma + %trim(pCamr)    + $Coma + ' And ' +                             
              'PcgInd = ''N''';                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor2 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor2 Scroll Cursor For Cursor2;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor2;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor2 Into :WPcgcdg, :WPcgdes, :WPcgcdp, :WPcgInd;                                 
                                                                                                    
       Dow  sqlcod=*Zeros;                                                                          
        PrVlrPagar();                                                                               
        PrMotPgRete();                                                                              
        If WPcgInd = 'N';                                                                           
         PrGrabar();                                                                                
        Endif;                                                                                      
        Exec Sql                                                                                    
          fetch Cursor2 Into :WPcgcdg, :WPcgdes, :WPcgcdp, :WPcgInd;                                
       EndDo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close cursor2;                                                                             
      /End-Free                                                                                     
     PPrVttdngf        E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Graba Registro  en archivo Vttdngf                  //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrGrabar         B                                                                            
     DPrGrabar         Pi                                                                           
      /Free                                                                                         
        W_Cia = ICia;                                                                               
        W_Reg = ORegion;                                                                            
        W_Zona = IZona;                                                                             
        W_Camp = ICampa;                                                                            
                                                                                                    
        chain (W_Cia:W_Reg:W_Zona:IsectorG:IcedulaG:PcamR:WPcgcdp) Vttdngf;                         
         If Not %Found(Vttdngf);                                                                    
             Dngest  = 'E';                                                                         
             Dngcia  = W_Cia;                                                                       
             Dngreg  = W_Reg;                                                                       
             Dngzng  = W_Zona;                                                                      
             Dngscg  = IsectorG;                                                                    
             Dngcdg  = IcedulaG;                                                                    
             Dngcmp  = PcamR;                                                                       
             Dngvlr  = WDngvlr;                                                                     
             Dngcdr  = WPcgcdp;                                                                     
             Dngmot  = X_motiv;                                                                     
             Dngaut  = ' ';                                                                         
             Dngfec  = %Char(%Date());                                                              
             Dnghor  = %Char(%Time());                                                              
             Dngusr  = %Trim(W_USer);                                                               
             If Dngmot <> ' ';                                                                      
              Write    RegDng;                                                                      
             Endif;                                                                                 
         Else;                                                                                      
             Dngvlr  = WDngvlr;                                                                     
             Dngmot  = X_motiv;                                                                     
             Dngaut  = ' ';                                                                         
             Dngfec  = %Char(%Date());                                                              
             Dnghor  = %Char(%Time());                                                              
             Dngusr  = %Trim(W_USer);                                                               
             If Dngmot <> ' ';                                                                      
              Update RegDng;                                                                        
             Endif;                                                                                 
         Endif;                                                                                     
      /End-Free                                                                                     
     PPrGrabar         E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Recupera Valor a pagar                              //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrVlrPagar       B                                                                            
     DPrVlrPagar       Pi                                                                           
      /Free                                                                                         
       XValor = *Zeros;                                                                             
                                                                                                    
       Wsql = *Blanks;                                                                              
       Wsql = 'Select IfNull(SpgVps, 0) ' +                                                         
              'From VotreA00/VtmSpgf ' +                                                            
              'Where Spgcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'SpgZon = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Spgcam = ' + $Coma + %trim(PCamr)    + $Coma + ' And ' +                             
              'Spgsec = ' + $Coma + %trim(ISectorG)  + $Coma + ' And ' +                            
              'Spgced = ' + %trim(%char(ICedulaG))           + ' And ' +                            
              'SpgTrr = '' ''';                                                                     
       Exec Sql                                                                                     
         Prepare Cursor3 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor3 Scroll Cursor For Cursor3;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor3;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor3 Into :XValor;                                                                
                                                                                                    
       WDngvlr = *Zeros;                                                                            
       IF sqlcod=*Zeros;                                                                            
       WDngvlr = XValor;                                                                            
       EndIF;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close cursor3;                                                                             
      /End-Free                                                                                     
     PPrVlrPagar       E                                                                            
                                                                                                    
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Recupera Motivo de Pago Retenido                    //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrMotPgRete      B                                                                            
     DPrMotPgRete      Pi                                                                           
      /Free                                                                                         
       W_motiv = *Blanks;                                                                           
       Wsql = *Blanks;                                                                              
       Wsql = 'Select DngDes ' +                                                                    
              'From VotreA00/Vtmdngf ' +                                                            
              'Where Dngcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'Dngcod = ' + %trim(%char(WPcgcdp)) + ' And ' +                                       
              'Dngest = ' + '''' + ' ' + '''';                                                      
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor4 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor4 Scroll Cursor For Cursor4;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor4;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor4 Into :W_motiv;                                                               
                                                                                                    
       X_motiv = *Blanks;                                                                           
       IF sqlcod=*Zeros;                                                                            
       X_motiv = W_motiv ;                                                                          
       EndIF;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close cursor4;                                                                             
      /End-Free                                                                                     
     PPrMotPgRete      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba los Casos Pendientes Pago Guía por Falta //                       
       // del RUT                                                          //                       
       //------------------------------------------------------------------//                       
     PPrPagosPendientesGuias...                                                                     
     P                 B                                                                            
     DPrPagosPendientesGuias...                                                                     
     D                 Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DNro              s              3  0  Inz(*Zeros)                                             
     DYnro             s              3  0  Inz(*Zeros)                                             
     DYcia             s              3a   Inz(*Blanks)                                             
     DTced             s             30a   Inz(*Blanks)                                             
     DTcns             s              9a   Inz(*Blanks)                                             
     DTvalor           s             11  2 Inz(*Zeros)                                              
      /Free                                                                                         
       Ycia = Icia;                                                                                 
       Tced = %Trim(%Editc(ICedulaG:'Z'));                                                          
       Tcns = *Blanks;                                                                              
       Conversion(Ycia:Tced:Tcns);                                                                  
       Ynro = *Zeros;                                                                               
       Exec Sql                                                                                     
        Select Count(0) Into :Ynro                                                                  
        From Votrea00/Vtmspgf                                                                       
        Where Spgcia = :Icia And                                                                    
              Spgzon = :Izona And                                                                   
              Spgcam = :PCamR And                                                                   
              Spgsec = :ISectorG;                                                                   
       If Ynro > *Zeros;                                                                            
          Exec Sql                                                                                  
           Select Count(0) Into :Nro                                                                
           From Votrea00/Vtmrtgf                                                                    
           Where Trim(Char(Rtgidt)) = Trim(:Tced)                                                   
                 And Rtgcia = :Icia                                                                 
                 And (Rtgest = ' ' Or (Rtgest = 'D' And (Rtgcdt =                                   
                 Trim(:PCamR) Or Rtgcdt = Trim(:ICamPa))));                                         
        If Nro > 0;                                                                                 
         Exec Sql                                                                                   
          Delete From VotreA00/Vttpprf                                                              
          Where Pprcia = :Icia And                                                                  
                Pprcdg = :ICedulaG And                                                              
                Pprzna = :Izona And                                                                 
                Pprsec = :ISectorG And Pprcmp = :PCamR;                                             
                                                                                                    
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrPagosPendientesGuias...                                                                     
     P                 E                                                                            
