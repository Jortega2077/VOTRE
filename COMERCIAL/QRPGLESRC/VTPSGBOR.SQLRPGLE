***************************************************************************                         
     ** Programa      : VTPSGBOR                                          *                         
     ** Descripción:  : PGM Que Valida si Cumplio las politicas definidas *                         
     **                 por el area comercial para poder realizar el pago *                         
     **                 por campaña                                       *                         
     ** Proyecto      : Nuevo Plan de Guías                               *                         
     ** Autor         : John William Palacio PersonalSoft                 *                         
     ** Fecha Creación: 30 de Junio de 2016                               *                         
     **********************************************************************                         
                                                                                                    
      ** Definición de Directivas de Compilación                                                    
       Ctl-Opt Dftactgrp(*no) Bnddir('QC2LE');                                                      
                                                                                                    
      ** Definición de archivos de Trabajo                                                          
       Dcl-f Vtwpcgf Disk Usage(*Input:*Output) Keyed;                                              
       Dcl-f Lorort01 Disk Usage(*Input) Keyed;                                                     
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
       Dcl-s W_Cedula Packed(9:0) Inz(*Zeros);                                                      
       Dcl-s W_Orden Packed(9:0) Inz(*Zeros);                                                       
       Dcl-s W_NroPd Packed(9:0) Inz(*Zeros);                                                       
       Dcl-s W_OhoRty Char(3) Inz(*Blanks);                                                         
       Dcl-s W_Region Char(10) Inz(*Blanks);                                                        
       Dcl-s W_Region1 Char(30) Inz(*Blanks);                                                       
       Dcl-s W_Cedul Char(10) Inz(*Blanks);                                                         
       Dcl-s W_Omail Char(10) Inz(*Blanks);                                                         
       Dcl-s W_Usr Char (10) Inz(*User);                                                            
       Dcl-s WSql Char (1000) Inz(*User);                                                           
       Dcl-s W_Des Char (200) Inz(*User);                                                           
       Dcl-s $COMA Char(1) Inz('''');                                                               
       Dcl-s W_Valor Packed(11:2) Inz(*Zeros);                                                      
       Dcl-s W_ValCon Char(11);                                                                     
       Dcl-s W_ValConN Packed(11:2) Inz(*Zeros);                                                    
       Dcl-s W_Resta Packed(11:2) Inz(*Zeros);                                                      
       Dcl-s W_Ind Char(1);                                                                         
       Dcl-s PCamE Char(4);                                                                         
       Dcl-s PCamR Char(4);                                                                         
       Dcl-s PcamTemp Char(4);                                                                      
       Dcl-s POper Char(1);                                                                         
       Dcl-s Pnume Char(3);                                                                         
       Dcl-s W_Cod  Packed(4:0) Inz(0002);                                                          
       Dcl-s IIwsw Ind Inz(*Off);                                                                   
                                                                                                    
      ** Definición de Procedimientos                                                               
       Dcl-Pr PrVtmpdwf End-Pr;                                                                     
       Dcl-Pr PrVtwpcgf End-Pr;                                                                     
       Dcl-Pr PrVtmpdpf End-Pr;                                                                     
       Dcl-Pr PrVtmpptf End-Pr;                                                                     
       Dcl-Pr PrVtmspsf End-Pr;                                                                     
       Dcl-Pr PrPedidoFacturado Packed(9:0) End-Pr;                                                 
       Dcl-Pr PrRangoLiberacionCartera Packed(14:2) End-Pr;                                         
       Dcl-Pr PrInsuficiencia Packed(9:0) End-Pr;                                                   
                                                                                                    
      ** Definición de Pgm Externos                                                                 
       Dcl-Pr PgmRegion Extpgm('VOTREP00/VTMGOIGR');                                                
         Xcia Char(3);                                                                              
         Xzona Char(3);                                                                             
         Xregion Char(10);                                                                          
         Xcedula Char(10);                                                                          
         XmailPlan Char(10);                                                                        
       End-Pr;                                                                                      
                                                                                                    
       Dcl-Pr ProCampa Extpgm('VOTREP00/VTMCSBKR');                                                 
         Pcia Char(3);                                                                              
         PCamE Char(4);                                                                             
         PCamR Char(4);                                                                             
         POper Char(1);                                                                             
         Pnume Char(3);                                                                             
       End-Pr;                                                                                      
                                                                                                    
       Dcl-Pr PgmValor Extpgm('VOTREP00/VTMPGBIR');                                                 
         Xcia Char(3);                                                                              
         Xregion Char(30);                                                                          
         Xzona Char(3);                                                                             
         XSector Char(3);                                                                           
         Xcampana Char(4);                                                                          
         XValor Packed(11:2);                                                                       
         XIndicador Char(1);                                                                        
       End-Pr;                                                                                      
                                                                                                    
       Dcl-Pr PgmValorCon Extpgm('VOTREP00/VTMCSB2R');                                              
         Xcia Char(3);                                                                              
         XCedula Char(9);                                                                           
         XValor Char(11);                                                                           
       End-Pr;                                                                                      
      ** Definición de estructuras                                                                  
                                                                                                    
      ** Definición de Procedimientos Parametros de Entrada                                         
       Dcl-Pr Main Extpgm('VTPSGBOR');                                                              
         PCia Char(3);                                                                              
         PCmp Char(4);                                                                              
         Pzona Char(3);                                                                             
         Pcedula Char(9);                                                                           
         PSector Char(3);                                                                           
       End-Pr;                                                                                      
       Dcl-PI Main;                                                                                 
         PCia Char(3);                                                                              
         PCmp Char(4);                                                                              
         Pzona Char(3);                                                                             
         Pcedula Char(9);                                                                           
         PSector Char(3);                                                                           
       End-PI;                                                                                      
                                                                                                    
       //------------------------------------------------------------------/                        
       // Inicio Programa Principal                                        /                        
       //------------------------------------------------------------------/                        
        PCamE = Pcmp;                                                                               
        PCamR = *Blanks;                                                                            
        POper = '-';                                                                                
        Pnume = '1';                                                                                
        ProCampa(Pcia:PCamE:PCamR:POper:Pnume);                                                     
        PcamTemp = PCamR;                                                                           
                                                                                                    
        W_Cedula = %Int(PCedula);                                                                   
        CallP(E) PgmRegion(PCia:PZona:W_Region:W_Cedul:W_Omail);                                    
        W_Region1 = W_Region;                                                                       
        Clear W_Orden;                                                                              
        IIwsw = *Off;                                                                               
        W_Orden = PrPedidoFacturado();                                                              
        If IIwsw = *Off;                                                                            
         PrVtmpdwf();                                                                               
         PrVtmpdpf();                                                                               
         PrVtmpptf();                                                                               
         PrVtmspsf();                                                                               
        ElseIf IIwsw = *On;                                                                         
         PcgInd = 'S';                                                                              
         PcgCdp = 0002;                                                                             
         PrVtwpcgf();                                                                               
        Endif;                                                                                      
        *Inlr = *On;                                                                                
        Return;                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento encargado de verificar si la guía realizo un pedido//                       
       // en la campaña y este fue echo por la WEB                         //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrVtmpdwf;                                                                          
        Clear W_NroPd;                                                                              
        Exec Sql                                                                                    
          Select Count(0) Into :W_NroPd                                                             
          From Votrea00/Vtglmgf                                                                     
          Where LmgCia = :PCia And                                                                  
                LmgMed = :PCmp And                                                                  
                //LmgZon = :PZona And                                                               
                LmgCod = :W_Cedula And                                                              
                LmgTfa <> 0 ;                                                                       
                                                                                                    
        If W_NroPd > *Zeros;                                                                        
          If W_Orden <> *Zeros;                                                                     
            Chain (W_OhoRty) Lorort01;                                                              
            If %Found(Lorort01);                                                                    
              If OooRty = '003' Or OooRty = '005' Or OooRty = '008'                                 
                 Or OooRty = '011';                                                                 
                PcgInd = 'S';                                                                       
                PcgCdp = 0001;                                                                      
                PrVtwpcgf();                                                                        
              Else;                                                                                 
                PcgInd = 'N';                                                                       
                PcgCdp = 0001;                                                                      
                PrVtwpcgf();                                                                        
              EndIf;                                                                                
            Else;                                                                                   
              PcgInd = 'N';                                                                         
              PcgCdp = 0001;                                                                        
              PrVtwpcgf();                                                                          
            EndIf;                                                                                  
          EndIf;                                                                                    
        Else;                                                                                       
          PcgInd = 'N';                                                                             
          PcgCdp = 0002;                                                                            
          PrVtwpcgf();                                                                              
        EndIf;                                                                                      
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si el pedido esta al menos facturado  //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrPedidoFacturado;                                                                  
        Dcl-Pi *N Packed(9:0) End-Pi;                                                               
        Dcl-s W_Nro Packed(9:0) Inz(*Zeros);                                                        
                                                                                                    
        Wsql = *Blanks;                                                                             
        Clear W_OhoRty;                                                                             
        Wsql = 'Select OhoRdÑ, OhoRty ' +                                                           
               'From Opf' + %Trim(Pcia) + '/PorHdr ' +                                              
               'Where OhcStÑ = ' + %Trim(%Editc(W_Cedula:'Z')) + ' And ' +                          
               'OhmEda = ' + $Coma + %Trim(PCmp) + $Coma + ' And ' +                                
               'OhcMfl = ''N'' And OhrCst = ''S'' ' +                                               
               'Group By Trim(OhmEda), OhoRdÑ, OhoRty, Rrn(PorHdr) ' +                              
               'Order By Trim(OhmEda), OhoRdÑ, Rrn(PorHdr) Asc ' +                                  
               'Fetch First 1 Row Only';                                                            
                                                                                                    
        Exec Sql                                                                                    
          Prepare Cu1 From :Wsql;                                                                   
                                                                                                    
        Exec Sql                                                                                    
          Declare Cu1 Cursor For Cu1;                                                               
                                                                                                    
        Exec Sql                                                                                    
          Open Cu1;                                                                                 
                                                                                                    
        Exec Sql                                                                                    
          Fetch Cu1 Into :W_Nro, :W_OhoRty;                                                         
                                                                                                    
        Exec Sql                                                                                    
          Close Cu1;                                                                                
                                                                                                    
        If W_Nro = *Zeros;                                                                          
          W_Nro = PrInsuficiencia();                                                                
        Else;                                                                                       
         IIwsw = *On;                                                                               
        Endif;                                                                                      
                                                                                                    
        Return W_Nro;                                                                               
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si el pedido esta cancelado por       //                       
       // Insuficiencia de Mercancia                                       //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrInsuficiencia;                                                                    
        Dcl-Pi *N Packed(9:0) End-Pi;                                                               
        Dcl-s W_Nro Packed(9:0) Inz(*Zeros);                                                        
        Dcl-s W_NroX Packed(9:0) Inz(*Zeros);                                                       
        Dcl-s W_Num Packed(9:0) Inz(*Zeros);                                                        
                                                                                                    
        Wsql = *Blanks;                                                                             
        Wsql = 'Select OhoRdÑ, OhoRty ' +                                                           
               'From Opf' + %Trim(Pcia) + '/PorHdr ' +                                              
               'Where OhcStÑ = ' + %Trim(%Editc(W_Cedula:'Z')) + ' And ' +                          
               'OhmEda = ' + $Coma + %Trim(PCmp) + $Coma + ' And ' +                                
               'OhcMfl = ''N'' And OhrCst = ''C'' ' +                                               
               'Group By Trim(OhmEda), OhoRdÑ, OhoRty, Rrn(PorHdr) ' +                              
               'Order By Trim(OhmEda), OhoRdÑ, Rrn(PorHdr) Asc ' +                                  
               'Fetch First 1 Row Only';                                                            
                                                                                                    
        Exec Sql                                                                                    
          Prepare Cu2 From :Wsql;                                                                   
                                                                                                    
        Exec Sql                                                                                    
          Declare Cu2 Cursor For Cu2;                                                               
                                                                                                    
        Exec Sql                                                                                    
          Open Cu2;                                                                                 
                                                                                                    
        Exec Sql                                                                                    
          Fetch Cu2 Into :W_NroX, :W_OhoRty;                                                        
                                                                                                    
        If SqlCode <> 100 And SqlCode >= *Zeros;                                                    
          Clear Wsql;                                                                               
          Wsql = 'Select Count(Distinct(OdoRdÑ)) ' +                                                
                 'From Opf' + %Trim(Pcia) + '/PorDtl ' +                                            
                 'Where OdsCus = ' + %Trim(%Editc(W_Cedula:'Z')) + ' And ' +                        
                 'OdoRdÑ = ' + %Trim(%Editc(W_NroX:'Z')) + ' And ' +                                
                 '(OdlCcd = ''INS'' Or OdlCcd = ''BAC'' Or OdlCcd = ''CCD'' )';                     
                                                                                                    
          Exec Sql                                                                                  
            Prepare Cu3 From :Wsql;                                                                 
                                                                                                    
          Exec Sql                                                                                  
            Declare Cu3 Cursor For Cu3;                                                             
                                                                                                    
          Exec Sql                                                                                  
            Open Cu3;                                                                               
                                                                                                    
          Exec Sql                                                                                  
            Fetch Cu3 Into :W_Num;                                                                  
                                                                                                    
          If W_Num > *Zeros;                                                                        
           W_Nro = W_NroX;                                                                          
           IIwsw = *On;                                                                             
          EndIf;                                                                                    
                                                                                                    
          Exec Sql                                                                                  
            Close Cu3;                                                                              
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
          Close Cu2;                                                                                
                                                                                                    
        Return W_Nro;                                                                               
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que se encarga de Grabar en el archivo VTWPCGF     //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrVtwpcgf;                                                                          
        Dcl-s Inro Zoned(5:0);                                                                      
                                                                                                    
        Clear Inro;                                                                                 
        Exec Sql                                                                                    
         Select Count(0) Into :Inro                                                                 
         From Votrea00/Vtmdngf                                                                      
         Where Dngcia = :PCia And                                                                   
               Dngcod = :PcgCdp And                                                                 
               Dngest = ' ';                                                                        
                                                                                                    
         If Inro > 0;                                                                               
         //Chain (PCia:PcamTemp:W_Cedula:W_Cod) Vtwpcgf;                                            
         Chain (PCia:PcamTemp:W_Cedula:PcgCdp) Vtwpcgf;                                             
         If Not %Found(Vtwpcgf);                                                                    
           PcgCia = PCia;                                                                           
           PcgReg = W_Region;                                                                       
           PcgZna = Pzona;                                                                          
           PcgSct = PSector;                                                                        
           PcgCmp = PcamTemp;                                                                       
           PcgCdg = W_Cedula;                                                                       
           Exec Sql                                                                                 
             Select DngDes Into :W_Des                                                              
             From Votrea00/Vtmdngf                                                                  
             Where DngCia = :PCia And                                                               
                   DngCod = :PcgCdp And                                                             
                   Dngest = ' ';                                                                    
                                                                                                    
           PcgDes = W_Des;                                                                          
           PcgFec = %Char(%Date);                                                                   
           PcgHor = %Char(%Time);                                                                   
           PcgUsr = W_Usr;                                                                          
           Write(E) RegPcg;                                                                         
         EndIf;                                                                                     
         EndIf;                                                                                     
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento encargado de verificar si se realizo un pedido     //                       
       // en la campaña y su valor facturado sea mayor o igual a 150.000   //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrVtmpdpf;                                                                          
                                                                                                    
        CallP(E) PgmValor(PCia:W_Region1:PZona:PSector:Pcmp:W_Valor:W_Ind);                         
        If W_Ind = 'S';                                                                             
          Clear W_NroPd;                                                                            
          Exec Sql                                                                                  
            Select Count(0) Into :W_NroPd                                                           
            From Votrea00/Vtglmgf                                                                   
            Where LmgCia = :PCia And                                                                
                  Lmgmed = :Pcmp And                                                                
                //LmgZon = :PZona And                                                               
               // LmgSec = :PSector And                                                             
                  LmgCod = :W_Cedula And                                                            
                  LmgTfa <> 0 And                                                                   
                  LmgTvp >= :W_valor;                                                               
                                                                                                    
          If W_NroPd > *Zeros;                                                                      
            PcgInd = 'S';                                                                           
            PcgCdp = 0003;                                                                          
            PrVtwpcgf();                                                                            
          Else;                                                                                     
            PcgInd = 'N';                                                                           
            PcgCdp = 0003;                                                                          
            PrVtwpcgf();                                                                            
          EndIf;                                                                                    
        Else;                                                                                       
          PcgInd = 'N';                                                                             
          PcgCdp = 0003;                                                                            
          PrVtwpcgf();                                                                              
        EndIf;                                                                                      
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento encargado de verificar si la guía tiene pedido     //                       
       // en la campaña                                                    //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrVtmpptf;                                                                          
                                                                                                    
                                                                                                    
        Clear W_NroPd;                                                                              
        Exec Sql                                                                                    
          Select Count(0) Into :W_NroPd                                                             
          From Votrea00/Vtglmgf                                                                     
          Where LmgCia = :PCia And                                                                  
                Lmgmed = :PCmp And                                                                  
              //LmgZon = :PZona And                                                                 
              //LmgSec = :PSector And                                                               
                LmgCod = :W_Cedula And                                                              
                LmgTfa <> 0;                                                                        
                                                                                                    
        If W_NroPd > *Zeros;                                                                        
          PcgInd = 'S';                                                                             
          PcgCdp = 0002;                                                                            
          PrVtwpcgf();                                                                              
        Else;                                                                                       
          PcgInd = 'N';                                                                             
          PcgCdp = 0002;                                                                            
          PrVtwpcgf();                                                                              
        EndIf;                                                                                      
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento encargado de verificar que la guía este a paz      //                       
       // y salvo con Leonisa                                              //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrVtmspsf;                                                                          
                                                                                                    
        Clear W_NroPd;                                                                              
        Clear W_Valor;                                                                              
                                                                                                    
        Exec Sql                                                                                    
          Select Count(0), Sum(LmgTfa) Into :W_NroPd, :W_Valor                                      
          From Votrea00/Vtglmgf                                                                     
          Where LmgCia = :PCia And                                                                  
                Lmgmed = :Pcmp And                                                                  
              //LmgZon = :PZona And                                                                 
              //LmgSec = :PSector And                                                               
                LmgCod = :W_Cedula And                                                              
                LmgTfa <> 0;                                                                        
                                                                                                    
        If W_NroPd > *Zeros;                                                                        
          Clear W_Resta;                                                                            
          If W_Orden <> *Zeros;                                                                     
            CallP(E) PgmValorCon(PCia:PCedula:W_ValCon);                                            
              Clear W_ValConN;                                                                      
     C                   Movel     W_ValCon      W_ValConN                                          
              If W_ValConN > W_Valor;                                                               
                W_Resta = %Int(W_ValCon) - W_Valor;                                                 
                If W_Resta > PrRangoLiberacionCartera();                                            
                  PcgInd = 'S';                                                                     
                  PcgCdp = 0004;                                                                    
                  PrVtwpcgf();                                                                      
                Else;                                                                               
                  PcgInd = 'N';                                                                     
                  PcgCdp = 0004;                                                                    
                  PrVtwpcgf();                                                                      
                EndIf;                                                                              
              EndIf;                                                                                
          EndIf;                                                                                    
        EndIf;                                                                                      
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el rango de liberación de Cartera     //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrRangoLiberacionCartera;                                                           
        Dcl-Pi *N Packed(14:2) End-Pi;                                                              
        Dcl-s W_Custom Char(3) Inz(*Blanks);                                                        
        Dcl-s W_Custom1 Char(3) Inz('N');                                                           
        Dcl-s W_Mlinf1 Char(10) Inz(*Blanks);                                                       
        Dcl-s W_Mldm01 Char(1) Inz(*Blanks);                                                        
        Dcl-s W_XValor Packed(14:2) Inz(*Zeros);                                                    
                                                                                                    
        Clear W_Mlinf1;                                                                             
        Clear W_Mldm01;                                                                             
        Wsql = *Blanks;                                                                             
        Wsql = 'Select MlcLas, MldM01, MliNf1 ' +                                                   
               'From Opcapf/Pmllst' + %Trim(PCia) +                                                 
               ' Where MlcStÑ = ' + %Trim(%Editc(W_Cedula:'Z')) +                                   
               ' Fetch First 1 Row Only';                                                           
                                                                                                    
        Exec Sql                                                                                    
          Prepare Cu4 From :Wsql;                                                                   
                                                                                                    
        Exec Sql                                                                                    
          Declare Cu4 Cursor For Cu4;                                                               
                                                                                                    
        Exec Sql                                                                                    
          Open Cu4;                                                                                 
                                                                                                    
        Exec Sql                                                                                    
          Fetch Cu4 Into :W_Custom, :W_Mlinf1, :W_Mldm01;                                           
                                                                                                    
        If SqlCode <> 100 And SqlCode >= *Zeros;                                                    
          Clear Wsql;                                                                               
          Wsql = 'Select RanRli ' +                                                                 
                 'From Votrea' + %Trim(Pcia) + '/Vtmrlif ' +                                        
                 'Where RanTip = ' + $Coma + W_Custom + $Coma ;                                     
                                                                                                    
          Exec Sql                                                                                  
            Prepare Cu5 From :Wsql;                                                                 
                                                                                                    
          Exec Sql                                                                                  
            Declare Cu5 Cursor For Cu5;                                                             
                                                                                                    
          Exec Sql                                                                                  
            Open Cu5;                                                                               
                                                                                                    
          Exec Sql                                                                                  
            Fetch Cu5 Into :W_XValor;                                                               
                                                                                                    
          If SqlCode = 100 Or SqlCode < *Zeros;                                                     
            Clear Wsql;                                                                             
            Wsql = 'Select RanRli ' +                                                               
                   'From Votrea' + %Trim(Pcia) + '/Vtmrlif ' +                                      
                   'Where RanTip = ' + $Coma + W_Custom1 + $Coma ;                                  
                                                                                                    
            Exec Sql                                                                                
              Prepare Cu6 From :Wsql;                                                               
                                                                                                    
            Exec Sql                                                                                
              Declare Cu6 Cursor For Cu6;                                                           
                                                                                                    
            Exec Sql                                                                                
              Open Cu6;                                                                             
                                                                                                    
            Exec Sql                                                                                
              Fetch Cu6 Into :W_XValor;                                                             
                                                                                                    
            If SqlCode = 100 Or SqlCode < *Zeros;                                                   
              W_XValor = *Zeros;                                                                    
            EndIf;                                                                                  
                                                                                                    
            Exec Sql                                                                                
              Close Cu6;                                                                            
                                                                                                    
          EndIf;                                                                                    
                                                                                                    
          Exec Sql                                                                                  
            Close Cu5;                                                                              
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
          Close Cu4;                                                                                
                                                                                                    
        Return W_XValor;                                                                            
                                                                                                    
       End-Proc;                                                                                    
