      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCC15R                                         **                        
      * Descripcion   : Generación Informe Estado de Cuenta Portal Lidere **                        
      * Proyecto      : O.P.I Paises                                      **                        
      * Autor         : JLOPEZL                                           **                        
      * Fecha creacion: Septiembre 10 de 2009                             **                        
      **********************************************************************                        
      * Modificación  : Se cambia el el logico LARDTL01 por su respectivo  *                        
      *                 Fisico PARDTL                                      *                        
      * Autor         : John William Palacio Cardenas PersonalSoft         *                        
      * Fecha         : 02 de septiembre de 2014                           *                        
      * Requerimiento : Estado de Cuentas                                 **                        
      **********************************************************************                        
      * Modificación  : Se Agrega el campo de Pago anticipado              *                        
      * Autor         : John William Palacio Cardenas PersonalSoft         *                        
      * Fecha         : 04 de Marzo de 2015                                *                        
      * Requerimiento : Estado de Cuentas                                 **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definicion de Archivos de Trabajo                                                           
     FPARDTL    If   e           k Disk                                                             
     FPardtlhis If   e           k Disk                                                             
     FVthparf   If   e           k Disk     Rename(Pardtlhf1:Reg2)                                  
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     F*Vtcpci2p  Cf   e             Workstn                                                         
                                                                                                    
     ** Variables de Trabajo a Utilizar                                                             
     DDateFin          s               d                                                            
     DDateIni          s               d                                                            
     DFecdia           s              8a   Inz(*Blanks)                                             
     DContar           s              6s 0 Inz(*Zeros)                                              
     DFecX             s              8a   Inz(*Blanks)                                             
     DFecvX            s              8a   Inz(*Blanks)                                             
     DFecvXTemp        s              8a   Inz(*Blanks)                                             
     DFecfX            s              8a   Inz(*Blanks)                                             
     DFec              s              8s 0 Inz(*Zeros)                                              
     DFecv             s              8s 0 Inz(*Zeros)                                              
     DFecf             s              8s 0 Inz(*Zeros)                                              
     DValNeg           s             15s 2 Inz(*Zeros)                                              
     DValPos           s             15s 2 Inz(*Zeros)                                              
     DSaldo            s             15s 2 Inz(*Zeros)                                              
     DValNegA          s             15s 2 Inz(*Zeros)                                              
     DValPosA          s             15s 2 Inz(*Zeros)                                              
     DSaldoA           s             15s 2 Inz(*Zeros)                                              
     DSaldoTI          s             15s 2 Inz(*Zeros)                                              
     DSaldoTP          s             15s 2 Inz(*Zeros)                                              
     DArchiv           s             10a   Inz(*Blanks)                                             
     DArchiv1          s             10a   Inz(*Blanks)                                             
     DLibrer           s             10a   Inz(*Blanks)                                             
     DSqlStr           s           1000a                                                            
     DSqlStr1          s           1000a                                                            
     DSqlStr2          s           1000a                                                            
     DOrdenAux         s              9  0 Inz(*Zeros)                                              
     DOrdenAuxA        s              9  0 Inz(*Zeros)                                              
     DWtipo            s             20a                                                            
     DNroFac           s              6a   Inz(*Blanks)                                             
     DMEDA             s              4a   Inz(*Blanks)                                             
     DSeq              s             11s 0 Inz(*zeros)                                              
     DW_pago           s              2A   Inz(*Blanks)                                             
     DW_Conse          S              2  0 Inz(99)                                                  
     DW_Val            S             15  0 Inz(*Zeros)                                              
     DW_Dia            S              2  0 Inz(*Zeros)                                              
     DW_Acc            S              1A   Inz(*Blanks)                                             
     DW_Ord            S              9  0 Inz(*Zeros)                                              
     DW_Fec            S              8  0 Inz(*Zeros)                                              
     DW_Sec            S             11  0 Inz(*Zeros)                                              
     DW_Com            S             20A   Inz(*Blanks)                                             
     DP_Cia            S              3A   Inz(*Blanks)                                             
     DW_Blan           S              1A   Inz(*Blanks)                                             
     DW_Zero           S              1S 0 Inz(*Zeros)                                              
     DW_Text           S              8A   Inz(*Blanks)                                             
     DW_Fec1           S              8  0 Inz(*Zeros)                                              
     DW_Fec2           S              8  0 Inz(*Zeros)                                              
     DW_Ced            S              9  0 Inz(*Zeros)                                              
     DW_ValF           S             11  2 Inz(*Zeros)                                              
     DW_Ordt           S              9  0 Inz(*Zeros)                                              
     DW_PorCal         S             11  2 Inz(*Zeros)                                              
     DW_ValPago        S             11  2 Inz(*Zeros)                                              
     DW_Fecf           S              8  0 Inz(*Zeros)                                              
     DW_FecV           S              8  0 Inz(*Zeros)                                              
     DW_TotalPago      S             11  2 Inz(*Zeros)                                              
     DWMLZIPC          s             10                                                             
     DWdato            s             50                                                             
     DW                s              9  0                                                          
     DNull             s              1    Inz(X'00')                                               
     DPINDN            S              1A   Inz(*Blanks)                                             
     DHora             S             10A   Inz(*Blanks)                                             
     DFecha            S             10A   Inz(*Blanks)                                             
                                                                                                    
     DFecha2          sds                                                                           
     DSig_sys                195    196  0                                                          
     Dusuario                254    263                                                             
     DAÑo_sys                280    281  0                                                          
     DMes_sys                276    277  0                                                          
     DDia_sys                278    279  0                                                          
                                                                                                    
     DCia              S              3    Inz(*Blanks)                                             
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPCedula          s              9  0                                                          
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
                                                                                                    
     DPgmCastigo       Pr                  Extpgm('VOTREP00/VTMCCC10L')                             
     DP_Cia                           3A                                                            
     DP_Ced                           9  0                                                          
     DP_Con                           2  0                                                          
     DP_Ord                           9  0                                                          
     DP_Val                          15  0                                                          
     DP_Fec                           8  0                                                          
     DP_Dia                           2  0                                                          
     DP_Sec                          11  0                                                          
     DP_Com                          20A                                                            
     DP_Acc                           1A                                                            
                                                                                                    
     DPgmAestado       Pr                  Extpgm('VOTREP00/VTMCCCJ16L')                            
     DP_Cia                           3A                                                            
     DPZona                           3A                                                            
     DPIND                            1A                                                            
                                                                                                    
     DCedula           Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DTcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
     ** Parametros de Entrada al Programa                                                           
     DMain             Pr                  Extpgm('Main')                                           
     DCompania                        3a                                                            
     DPZona                           3a                                                            
     DP_Cedula                        9a                                                            
     DPIND                            1a   Options(*Nopass)                                         
                                                                                                    
     DMain             Pi                                                                           
     DCompania                        3a                                                            
     DPZona                           3a                                                            
     DP_Cedula                        9a                                                            
     DPIND                            1a   Options(*Nopass)                                         
                                                                                                    
     **************************************************************                                 
     *** Programa principal                                      **                                 
     **************************************************************                                 
                                                                                                    
      /Free                                                                                         
       If %Parms >= 4;                                                                              
                                                                                                    
        PCedula = %Int(P_Cedula);                                                                   
                                                                                                    
          Exec Sql                                                                                  
            Select MLZIPC Into :WMLZIPC                                                             
              From LMLLST01                                                                         
              Where MLCSTÑ  = :Pcedula                                                              
              Fetch first 1 rows Only;                                                              
                                                                                                    
          If Sqlcod = 100;                                                                          
             Hora    = %Char(%Time());                                                              
             Fecha   = %Char(%Date());                                                              
             Wdato = %trim(Compania) + '-' +                                                        
                         %trim(P_Cedula) + '-' +                                                    
                         'Cedula No existe Mlist: '+                                                
                         %trim(Fecha) +  ' '+ %trim(hora);                                          
             Exec Sql Insert into Gnpnalsof2/AudInsert values (:Wdato);                             
             *Inlr = *on;                                                                           
          EndIf;                                                                                    
                                                                                                    
        Monitor;                                                                                    
          PINDN = PIND;                                                                             
        On-error;                                                                                   
          Hora    = %Char(%Time());                                                                 
          Wdato = %trim(Compania) + '-' +                                                           
                      %trim(P_Cedula) + '-' +                                                       
                      %trim(PZona) + '-' +                                                          
                      'Entro al proceso ' + '-' +  %trim(hora) + '-' +                              
                      %trim(PINDN); // + ' fallo con : ' +  %trim(PIND);                            
          Exec Sql Insert into Gnpnalsof2/AudInsert values (:Wdato);                                
        EndMon;                                                                                     
                                                                                                    
        // Valida si hay error en indicador                                                         
        Monitor;                                                                                    
         If Pindn <> 'I' And Pindn <> 'L' and Pindn <> 'G';                                         
            Pindn = 'Z';                                                                            
            Wdato = %trim(Compania) + '-' +                                                         
                    %trim(P_Cedula) + '-' +                                                         
                    'Entro Indicador diferente ' + '-' +                                            
                    %trim(PIND    );                                                                
                                                                                                    
            Exec Sql Insert into Gnpnalsof2/AudInsert values (:Wdato);                              
         Endif;                                                                                     
        On-error;                                                                                   
          If PInd = Null;                                                                           
                                                                                                    
            Pind = 'X';                                                                             
            Wdato = %trim(Compania) + '-' +                                                         
                    %trim(P_Cedula) + '-' +                                                         
                    'Entro Nulo el indicador ' + '-' +                                              
                    %trim(PIND);                                                                    
                                                                                                    
            Exec Sql Insert into Gnpnalsof2/AudInsert values (:Wdato);                              
                                                                                                    
          Endif;                                                                                    
        EndMon;                                                                                     
                                                                                                    
          // Cuando se utiliza la opcion desde consulta integral                                    
          // se limpia zona para que la busque en LMLLST01                                          
          If PINDN = 'I';                                                                           
             Clear PZona;                                                                           
          Endif;                                                                                    
                                                                                                    
          // recupera zona                                                                          
          If PZona = *Blanks;                                                                       
                                                                                                    
             Exec Sql                                                                               
               Select MLZIPC Into :WMLZIPC                                                          
                 From LMLLST01                                                                      
                 Where MLCSTÑ  = :Pcedula                                                           
                 Fetch first 1 rows Only;                                                           
                                                                                                    
             Pzona = WMLZIPC;                                                                       
                                                                                                    
             If Pzona = ' ';                                                                        
                Pzona = '999';                                                                      
             Endif;                                                                                 
                                                                                                    
          Endif;                                                                                    
                                                                                                    
          Hora    = %Char(%Time());                                                                 
          Fecha   = %Char(%Date());                                                                 
          Wdato = %trim(Compania) + '-' +                                                           
                  %trim(PZona   ) + '-' +                                                           
                  %trim(P_Cedula) + '-' +                                                           
                  %trim(PINDN   ) + ' Paso OK '+                                                    
                  %trim(Fecha) +  ' '+ %trim(hora) + 'P00';                                         
                                                                                                    
          Exec Sql Insert into Gnpnalsof2/AudInsert values (:Wdato);                                
                                                                                                    
          Archiv = 'VTWE' + PINDN + PZona;                                                          
          Librer = 'VOTREA' + Compania;                                                             
          PgmPais(Compania:Xpais);                                                                  
          P_Cia = Compania;                                                                         
              If Pcedula <> *Zeros;                                                                 
                If Compania <> *Blanks;                                                             
                  P_Cia = Compania;                                                                 
                EndIf;                                                                              
                                                                                                    
                Exsr srBorrarFile;                                                                  
                Exsr srCrearFile;                                                                   
                Exsr srInsertarFile;                                                                
                Exsr srConCas;                                                                      
                Exsr srPagCas;                                                                      
                                                                                                    
                Sqlstr = *Blanks;                                                                   
                Sqlstr = 'Select Count(Cedula) As Contar ' +                                        
                         'From ' + %Trim(Librer) + '/' + %Trim(Archiv) ;                            
                                                                                                    
                Exec Sql                                                                            
                  Prepare Curs From :SqlStr;                                                        
                                                                                                    
                Exec Sql                                                                            
                  Declare Curso Cursor For Curs;                                                    
                                                                                                    
                Exec Sql                                                                            
                  Open Curso;                                                                       
                                                                                                    
                Exec Sql                                                                            
                  Fetch Curso Into :Contar;                                                         
                                                                                                    
                If Contar > *Zeros;                                                                 
                  Exsr srdiasMora;                                                                  
                                                                                                    
                  //Se Arman Archivos de Analisis del Estado                                        
                  PgmAestado(P_Cia:pzona:PINDN);                                                    
                                                                                                    
                Else;                                                                               
                   SqlStr = *Blanks;                                                                
                   SqlStr = 'Delete ' + %Trim(Librer) + '/' + %Trim(Archiv);                        
                                                                                                    
                   Exec Sql                                                                         
                     Prepare Cur From :SqlStr;                                                      
                                                                                                    
                    Exec Sql Execute Cur;                                                           
                                                                                                    
                   Archiv1 = 'VTWR' + Pindn + PZona;                                                
                   SqlStr = *Blanks;                                                                
                   SqlStr = 'Delete ' + %Trim(Librer) + '/' + %Trim(Archiv1);                       
                                                                                                    
                   Exec Sql                                                                         
                     Prepare Cur From :SqlStr;                                                      
                                                                                                    
                    Exec Sql                                                                        
                      Execute Cur;                                                                  
                                                                                                    
                Endif;                                                                              
                                                                                                    
                Exec Sql                                                                            
                  Close Curso;                                                                      
              Endif;                                                                                
        Compania = P_Cia;                                                                           
                                                                                                    
       EndIf;                                                                                       
        *Inlr = *On;                                                                                
                                                                                                    
         //************************************************************//                           
         //** Rutina que elimina tabla  Vtwgopf de la libreria del  ***//                           
         //** usuario que ejecuta el proceso                        ***//                           
         //************************************************************//                           
                                                                                                    
           Begsr srBorrarFile;                                                                      
            SqlStr = *Blanks;                                                                       
            SqlStr = 'Drop index ' + %Trim(Librer) + '/' + %Trim(Archiv) + 'L1';                    
                                                                                                    
            Exec Sql                                                                                
             Prepare Cur From :SqlStr;                                                              
                                                                                                    
            Exec Sql                                                                                
             Execute Cur;                                                                           
                                                                                                    
            SqlStr = *Blanks;                                                                       
            SqlStr = 'Drop Table ' + %Trim(Librer) + '/' + %Trim(Archiv);                           
                                                                                                    
            Exec Sql                                                                                
             Prepare Cur From :SqlStr;                                                              
                                                                                                    
            Exec Sql                                                                                
             Execute Cur;                                                                           
           Endsr;                                                                                   
                                                                                                    
                                                                                                    
         //************************************************************//                           
         //** Rutina que consulta y graba los Castigos              ***//                           
         //************************************************************//                           
                                                                                                    
           Begsr srConCas;                                                                          
            W_Acc = 'C';                                                                            
            PgmCastigo(P_Cia:Pcedula:W_Conse:W_Ord:W_Val:W_Fec:W_Dia:                               
                       W_Sec:W_Com:W_Acc);                                                          
                                                                                                    
            If W_Ord <> *Zeros;                                                                     
              W_Text = %Trim(%Char(W_Dia)) + ' CASTG';                                              
              SqlStr = 'Insert Into ' + %Trim(Librer) + '/' +                                       
                       %Trim(Archiv)+ ' (CompaÑia, Cedula, Orden, ' +                               
                       'Tipo, Vr_Factura, ' +                                                       
                       'Fecha_Facturacion, Dias_Mora, Seq, Comentario, ' +                          
                       'NroFactura, Fecha00002, pag_Antcp, Meda, Banco ) ' +                        
                       'Values( '                                                                   
                       + '''' + %Trim(P_Cia) + '''' + ', '                                          
                       + %Trim(%editc(Pcedula:'P')) + ', '                                          
                       + %Trim(%editc(W_ord:'P')) + ', '                                            
                       + '''' + 'Castigo' + '''' + ', '                                             
                       + %Trim(%editc(W_Val:'P')) + ', '                                            
                       + %Char(W_fec) + ', '                                                        
                       + '''' + %Trim(W_Text) +'''' + ', '                                          
                       + %Trim(%editc(W_Sec:'P')) + ', '                                            
                       + '''' + %Trim(W_Com) + '''' + ', '                                          
                       + '''' + %Trim(W_Blan) + '''' + ', '                                         
                       + %Trim(%editc(W_Zero:'P')) + ', '                                           
                       + '''' + %Trim(W_Blan) + '''' + ', '                                         
                       + '''' + %Trim(W_Blan) + '''' + ', '                                         
                       + '''' + %Trim(W_Blan) + '''' + ')';                                         
                                                                                                    
             Exec Sql                                                                               
               Prepare Cur2jI From :SqlStr;                                                         
                                                                                                    
             Exec Sql                                                                               
               Execute Cur2jI;                                                                      
             EndIf;                                                                                 
           Endsr;                                                                                   
                                                                                                    
         //************************************************************//                           
         //** Rutina que consulta y actualiza pagos para los castigos**//                           
         //************************************************************//                           
                                                                                                    
           Begsr srPagCas;                                                                          
            W_Acc = 'P';                                                                            
            Clear W_Val;                                                                            
            Clear W_Fec;                                                                            
            Clear W_Dia;                                                                            
            Clear W_Com;                                                                            
            PgmCastigo(P_Cia:Pcedula:W_Conse:W_Ord:W_Val:W_Fec:W_Dia:                               
                       W_Sec:W_Com:W_Acc);                                                          
                                                                                                    
            If W_Val <> *Zeros;                                                                     
              SqlStr = 'Update ' + %Trim(Librer) + '/' +                                            
                 %Trim(Archiv)+ ' Set Vr_pago = ' +                                                 
                 %Trim(%editc(W_Val:'P')) +                                                         
             // ', Saldo = Vr_Factura - ' + %Trim(%editc(W_Val:'P')) +                              
                ', Saldo=' +%Trim(%editc((-1*W_Val):'P')) + ' + Vr_Factura ' +                      
                ', Tipo = ''Castigo/Pago''' +                                                       
                ' Where TIPO = ''Castigo''';                                                        
                                                                                                    
              Exec Sql                                                                              
                Prepare Cur2jU From :SqlStr;                                                        
                                                                                                    
              Exec Sql                                                                              
                Execute Cur2jU;                                                                     
            Else;                                                                                   
              SqlStr = 'Update ' + %Trim(Librer) + '/' +                                            
                 %Trim(Archiv)+ ' Set Vr_pago = 0 ' +                                               
                ', Saldo = 0 ' +                                                                    
                ' Where TIPO = ''Castigo''';                                                        
                                                                                                    
              Exec Sql                                                                              
                Prepare Cur2jU From :SqlStr;                                                        
                                                                                                    
              Exec Sql                                                                              
                Execute Cur2jU;                                                                     
                                                                                                    
                                                                                                    
                                                                                                    
                                                                                                    
            EndIf;                                                                                  
           Endsr;                                                                                   
         //************************************************************//                           
         //** Rutina que crea    tabla  Estado Cta de la libreria del**//                           
         //** usuario que ejecuta el proceso                         **//                           
         //************************************************************//                           
                                                                                                    
           Begsr srCrearFile;                                                                       
            SqlStr = *Blanks;                                                                       
            SqlStr = 'Create Table ' + %Trim(Librer) + '/' +                                        
                 %Trim(Archiv)+ ' (CompaÑia CHAR(3), Cedula DECIMAL(9, 0), +                        
                      Orden DECIMAL(9, 0), NroFactura  CHAR(6), +                                   
                      Tipo CHAR(20),  +                                                             
                      Vr_Factura DECIMAL(11, 2), Vr_Pago DECIMAL(11, 2), +                          
                      Saldo DECIMAL(11, 2), +                                                       
                      Fecha_Facturacion DECIMAL(8, 0), +                                            
                      Fecha_Vencimiento DECIMAL(8, 0), +                                            
                      Dias_Mora CHAR(8), +                                                          
                      Pag_Antcp CHAR(2), +                                                          
                      MEDA    CHAR(4),   +                                                          
                      Seq     DECIMAL(11, 0), +                                                     
                      Banco CHAR(3), Comentario CHAR(20))   +                                       
                        RCDFMT ESTADO';                                                             
                                                                                                    
            Exec Sql                                                                                
              Prepare Cur1 From :SqlStr;                                                            
                                                                                                    
            Exec Sql                                                                                
              Execute Cur1;                                                                         
                                                                                                    
           Endsr;                                                                                   
         //************************************************************//                           
         //** Rutina que inserta datos en la tabla creada           ***//                           
         //************************************************************//                           
           begsr srInsertarFile;                                                                    
             Cia=%Trim(Compania);                                                                   
             W = 0;                                                                                 
             Exec Sql                                                                               
              Declare C2                                                                            
              Cursor For                                                                            
                                                                                                    
                                                                                                    
            Select Distinct ADCSTÑ, ADORDÑ, ADTRTY, ADTRAM, Fec, Fecf,                              
              Fecv, ADTEND, ADTNDÑ, ADRDPT, NroFac, MEDA, Seq                                       
            From (                                                                                  
              Select ADCSTÑ, ADORDÑ, ADTRTY, ADTRAM,                                                
          ( DIGITS(ADTRCC))||(DIGITS(ADTRYY))                                                       
                    ||(DIGITS(ADTRMM))||(DIGITS(ADTRDD)) as Fec,                                    
         (DIGITS(ADDSCC))||(DIGITS(ADDSYY))                                                         
                     ||(DIGITS(ADDSMM))||(DIGITS(ADDSDD)) as Fecf,                                  
         (DIGITS(ADDUCC))||(DIGITS(ADDUYY))                                                         
                     ||(DIGITS(ADDUMM))||(DIGITS(ADDUDD)) as Fecv,                                  
              ADTEND, ADTNDÑ, ADRDPT,                                                               
              CASE WHEN ADTRTY = 'I' THEN SUBSTR(ADDEPT, 5, 6)                                      
              ELSE ' '  END AS NroFac,                                                              
              SUBSTR(ADDEPT, 1, 4) MEDA,                                                            
              ADSEQÑ  Seq                                                                           
              From PARDTL                                                                           
              where ADCSTÑ = :Pcedula And  ADRDPT <> 014                                            
                                                                                                    
              Union all                                                                             
              Select ADCSTÑ, ADORDÑ, ADTRTY, ADTRAM,                                                
          ( DIGITS(ADTRCC))||(DIGITS(ADTRYY))                                                       
                    ||(DIGITS(ADTRMM))||(DIGITS(ADTRDD)) as Fec,                                    
         (DIGITS(ADDSCC))||(DIGITS(ADDSYY))                                                         
                     ||(DIGITS(ADDSMM))||(DIGITS(ADDSDD)) as Fecf,                                  
         (DIGITS(ADDUCC))||(DIGITS(ADDUYY))                                                         
                     ||(DIGITS(ADDUMM))||(DIGITS(ADDUDD)) as Fecv,                                  
              ADTEND, ADTNDÑ, ADRDPT,                                                               
              CASE WHEN ADTRTY = 'I' THEN SUBSTR(ADDEPT, 5, 6)                                      
              ELSE ' '  END AS NroFac,                                                              
              SUBSTR(ADDEPT, 1, 4) MEDA,                                                            
              ADSEQÑ  Seq                                                                           
              From pardtlhis                                                                        
              where ADCSTÑ = :Pcedula And  ADRDPT <> 014                                            
                                                                                                    
              Union all                                                                             
              Select ADCSTÑ, ADORDÑ, ADTRTY, ADTRAM,                                                
          ( DIGITS(ADTRCC))||(DIGITS(ADTRYY))                                                       
                    ||(DIGITS(ADTRMM))||(DIGITS(ADTRDD)) as Fec,                                    
         (DIGITS(ADDSCC))||(DIGITS(ADDSYY))                                                         
                     ||(DIGITS(ADDSMM))||(DIGITS(ADDSDD)) as Fecf,                                  
         (DIGITS(ADDUCC))||(DIGITS(ADDUYY))                                                         
                     ||(DIGITS(ADDUMM))||(DIGITS(ADDUDD)) as Fecv,                                  
              ADTEND, ADTNDÑ, ADRDPT,                                                               
              CASE WHEN ADTRTY = 'I' THEN SUBSTR(ADDEPT, 5, 6)                                      
              ELSE ' '  END AS NroFac,                                                              
              SUBSTR(ADDEPT, 1, 4) MEDA,                                                            
              ADSEQÑ  Seq                                                                           
              From Vthparf                                                                          
              where ADCSTÑ = :Pcedula And  ADRDPT <> 014) As aP                                     
              Order By Fec, ADORDÑ;                                                                 
                                                                                                    
            Exec Sql                                                                                
             Open C2;                                                                               
                                                                                                    
            Exec Sql                                                                                
               Fetch C2 Into :ADCSTÑ, :ADORDÑ, :ADTRTY, :ADTRAM,                                    
                        :Fec, :Fecf, :Fecv, :ADTEND,  :ADTNDÑ,                                      
                        :ADRDPT, :NroFac, :MEDA, :Seq;                                              
                                                                                                    
            SaldoTI = *Zeros;                                                                       
            SaldoTP = *Zeros;                                                                       
            ValNegA = *Zeros;                                                                       
            ValPosA = *Zeros;                                                                       
            SaldoA  = *Zeros;                                                                       
            Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                 
             If %Len(%Trim(%editc(Fec:'Z'))) > *Zeros;                                              
              FecX = %Trim(%editc(Fec:'Z'));                                                        
             Else;                                                                                  
              FecX = *All'0';                                                                       
             Endif;                                                                                 
             If Adtrty <> 'A';                                                                      
              If %Len(%Trim(%editc(Fecv:'Z'))) > *Zeros;                                            
               FecvX = %Trim(%editc(Fecv:'Z'));                                                     
              Else;                                                                                 
               FecvX = *All'0';                                                                     
              Endif;                                                                                
             ElseIf Adtrty <> 'A';                                                                  
              FecvX = %Trim(%editc(Fec:'Z'));                                                       
             Endif;                                                                                 
             If Adtrty <> 'A';                                                                      
              If %Len(%Trim(%editc(Fecf:'Z'))) > *Zeros;                                            
               FecfX = %Trim(%editc(Fecf:'Z'));                                                     
              Else;                                                                                 
               FecfX = *All'0';                                                                     
              Endif;                                                                                
             Else;                                                                                  
              FecfX = %Trim(%editc(Fec:'Z'));                                                       
             Endif;                                                                                 
               If  Adtram < *Zeros;                                                                 
                ValNeg = Adtram;                                                                    
                ValNegA += Adtram;                                                                  
               Else;                                                                                
                Valneg = *Zeros;                                                                    
               Endif;                                                                               
               If  Adtram > *Zeros;                                                                 
                Valpos = Adtram;                                                                    
                ValposA += Adtram;                                                                  
               Else;                                                                                
                Valpos = *Zeros;                                                                    
               Endif;                                                                               
                                                                                                    
              Saldo = *Zeros;                                                                       
              If %Scan('CASTIGO':%Trim(AdtndÑ))<=*Zeros And Adtrty = 'I';                           
               OrdenAuxA = AdordÑ;                                                                  
              Endif;                                                                                
              If ADTRTY = 'I' Or (ADTRTY = 'A' And ADTRAM > *Zeros);                                
               If OrdenAux <> AdordÑ;                                                               
                OrdenAux = AdordÑ;                                                                  
               Endif;                                                                               
               SaldoTI += Valpos;                                                                   
               If Adtrty = 'I';                                                                     
                Monitor;                                                                            
                 DateIni = %Date(%Subst(Fecvx:1:4) + '-' +                                          
                           %Subst(Fecvx:5:2) + '-' +                                                
                           %Subst(Fecvx:7:2));                                                      
                On-Error;                                                                           
                EndMon;                                                                             
                 Saldo = SaldoTI;                                                                   
               Endif;                                                                               
              Endif;                                                                                
              FecDia = *Blanks;                                                                     
              If ADTRTY = 'P' Or ADTRTY = 'C' Or                                                    
                 (ADTRTY = 'A' And ADTRAM <= *Zeros and ADRDPT <> 100 and                           
                  ADRDPT <> 101 And ADRDPT <> 102 And ADRDPT <> 103 And                             
                  ADRDPT <> 104);                                                                   
                                                                                                    
               SaldoTP = SaldoTI - (Valneg * (-1));                                                 
               SaldoTI = SaldoTP;                                                                   
                DateFin = %Date(%Subst(Fecx:1:4) + '-' +                                            
                          %Subst(Fecx:5:2) + '-' +                                                  
                          %Subst(Fecx:7:2));                                                        
                FecDia = %Char(%Diff(DateFin:DateIni:*Days));                                       
              Endif;                                                                                
              If AdordÑ = 888888888;                                                                
               Fecdia = *Blanks;                                                                    
              ElseIf AdordÑ <> 888888888 And FecDia <> ' ';                                         
               SqlStr = *Blanks;                                                                    
               SqlStr = 'Update ' + %Trim(Librer) + '/' +                                           
                        %Trim(Archiv)+ ' Set DIAS_MORA = ' +                                        
                        '''' + %Trim(Fecdia) + '''' + ' Where ' +                                   
                       'ORDEN = ' + %Trim(%editc(AdordÑ:'Z')) + ' ' +                               
                       'And TIPO = ''Factura''';                                                    
                                                                                                    
                Exec Sql                                                                            
                 Execute Immediate :SqlStr;                                                         
                                                                                                    
            If (ADTRTY = 'A' And %Scan('CASTIGO':%Trim(AdtndÑ))> *Zeros);                           
             SqlStr = *Blanks;                                                                      
             SqlStr = 'Update ' + %Trim(Librer) + '/' +                                             
                      %Trim(Archiv)+ ' Set DIAS_MORA = ' +                                          
                      '''' + %Trim(Fecdia) + '''' + ' Where ' +                                     
                      'ORDEN = ' + %Trim(%editc(OrdenAuxA:'Z')) + ' ' +                             
                      'And TIPO = ''Factura''';                                                     
                                                                                                    
              Exec Sql                                                                              
               Execute Immediate :SqlStr;                                                           
            Endif;                                                                                  
                                                                                                    
               Fecdia = *Blanks;                                                                    
              Endif;                                                                                
              If ADTRTY = 'P' Or ADTRTY = 'C' Or                                                    
                 (ADTRTY = 'A' And ADTRAM <= *Zeros                                                 
                                                                                                    
                                                    and ADRDPT <> 100 and                           
                  ADRDPT <> 101 And ADRDPT <> 102 And ADRDPT <> 103 And                             
                  ADRDPT <> 104);                                                                   
                                                                                                    
                                                                                                    
               Fecvx = *Zeros;                                                                      
              Endif;                                                                                
              Select;                                                                               
               When Adtrty = 'P';                                                                   
                Wtipo = 'Pago';                                                                     
               When Adtrty = 'I';                                                                   
                Wtipo = 'Factura';                                                                  
               When Adtrty = 'A';                                                                   
                Wtipo = 'Ajuste';                                                                   
               When Adtrty = 'C';                                                                   
                Wtipo = 'Nota Crédito';                                                             
              Endsl;                                                                                
              If Wtipo = 'Factura' And fecdia = *Blanks;                                            
               Fecdia = '0';                                                                        
              Endif;                                                                                
                                                                                                    
              If Wtipo = 'Factura';                                                                 
                If Fecx = Fecvx;                                                                    
                   W_pago = 'PA';                                                                   
                Else;                                                                               
                   W_pago = 'CR';                                                                   
                EndIf;                                                                              
              Else;                                                                                 
                Clear W_pago;                                                                       
              EndIf;                                                                                
                FecvXTemp = Fecvx;                                                                  
                If Adtrty <> 'I';                                                                   
                  Fecvx = '0';                                                                      
                EndIf;                                                                              
                                                                                                    
              SqlStr = *Blanks;                                                                     
              SqlStr = 'Insert Into ' + %Trim(Librer) + '/' +                                       
                       %Trim(Archiv)+ ' (CompaÑia, Cedula, Orden, +                                 
                       NroFactura, +                                                                
                       Tipo, Vr_Factura, Vr_Pago, Saldo, +                                          
                       Fecha_Facturacion, Fecha_Vencimiento, Dias_Mora, +                           
                       Pag_Antcp, MEDA,  Seq,  +                                                    
                       Banco, Comentario) Values('                                                  
                      + '''' + %Trim(P_Cia) + '''' +  ', '                                          
                      + %Trim(%editc(AdcstÑ:'Z')) + ', '                                            
                      + %Trim(%editc(AdordÑ:'Z')) + ', '                                            
                      + '''' + %Trim(NroFac) + '''' +  ', '                                         
                      + '''' + %Trim(Wtipo) + '''' +  ', '                                          
                      + %Trim(%editc(Valpos:'P')) + ', '                                            
                      + %Trim(%editc(Valneg:'P')) + ', '                                            
                      + %Trim(%editc(Saldo:'P')) + ', '                                             
                      + Fecx + ', ' + Fecvx + ', '                                                  
                      + '''' + %Trim(Fecdia) + '''' +  ', '                                         
                      + '''' + %Trim(W_Pago) + '''' +  ', '                                         
                      + '''' + %Trim(MEDA)   + '''' +  ', '                                         
                      + %Trim(%editc(Seq:'Z')) + ' , '                                              
                      + '''' + %Trim(Adtend) + '''' +  ', '                                         
                      + '''' + %Trim(AdtndÑ) + '''' +  ')';                                         
                                                                                                    
               Exec Sql                                                                             
                 Prepare Cur2 From :SqlStr;                                                         
                                                                                                    
               Exec Sql                                                                             
                 Execute Cur2;                                                                      
                                                                                                    
                 Fecvx = FecvXTemp;                                                                 
                                                                                                    
             Exec Sql                                                                               
               Fetch C2 Into :ADCSTÑ, :ADORDÑ, :ADTRTY,  :ADTRAM,                                   
                        :Fec, :Fecf, :Fecv, :ADTEND,     :ADTNDÑ,                                   
                        :ADRDPT, :NroFac, :MEDA, :Seq   ;                                           
                                                                                                    
             W += 1;                                                                                
             Enddo;                                                                                 
                                                                                                    
            If OrdenAux >= *Zeros And Adtrty <> 'I';                                                
             SqlStr = *Blanks;                                                                      
             SqlStr = 'Update ' + %Trim(Librer) + '/' + %Trim(Archiv) +                             
                      ' Set Saldo = ' + %Trim(%Editc(Saldo:'P')) + ' ' +                            
                      'Where Orden = ' + %Trim(%Editc(OrdenAux:'Z')) + ' And ' +                    
                      'Tipo = ''Factura''';                                                         
                                                                                                    
             Exec Sql                                                                               
              Prepare CurA From :SqlStr;                                                            
                                                                                                    
             Exec Sql                                                                               
              Execute CurA;                                                                         
            Endif;                                                                                  
                                                                                                    
             Exec Sql                                                                               
              Close C2;                                                                             
                                                                                                    
              If W = 0;                                                                             
                                                                                                    
                 compania = *Blanks;                                                                
                 AdcstÑ = *Zeros;                                                                   
                 AdordÑ = *Zeros;                                                                   
                 Wtipo  = 'Saldo';                                                                  
                 NROFAC = *Blanks;                                                                  
                 MEDA   = *Blanks;                                                                  
                 W_Pago = *Blanks;                                                                  
                 Seq    = *Zeros;                                                                   
                 Valpos = *Zeros;                                                                   
                 Valneg = *Zeros;                                                                   
                 Fecx   = '0';                                                                      
                 Fecvx  = '0';                                                                      
                 Fecdia = *Blanks;                                                                  
                 Adtend = *Blanks;                                                                  
                 AdtndÑ = *Blanks;                                                                  
                 SaldoA = *Zeros;                                                                   
                 SaldoA = ValposA - (ValnegA* (-1));                                                
                 SqlStr = *Blanks;                                                                  
                 SqlStr = 'Insert Into ' + %Trim(Librer) + '/' +                                    
                       %Trim(Archiv)+ ' (CompaÑia, Cedula, Orden, +                                 
                       Nrofactura,  +                                                               
                       Tipo, Vr_Factura, Vr_Pago, Saldo, +                                          
                       Fecha_Facturacion, Fecha_Vencimiento, Dias_Mora, +                           
                       Pag_Antcp, MEDA, Seq,  +                                                     
                       Banco, Comentario) Values('                                                  
                      + '''' + %Trim(P_Cia) + '''' +  ', '                                          
                      + %Trim(%editc(AdcstÑ:'P')) + ', '                                            
                      + %Trim(%editc(AdordÑ:'P')) + ', '                                            
                      + '''' + %Trim(NroFac) + '''' +  ', '                                         
                      + '''' + %Trim(Wtipo) + '''' +  ', '                                          
                      + %Trim(%editc(Valpos:'P')) + ', '                                            
                      + %Trim(%editc(Valneg:'P')) + ', '                                            
                      + %Trim(%editc(SaldoA:'P')) + ', '                                            
                      + Fecx + ', '                                                                 
                      + Fecvx + ', '                                                                
                      + '''' + %Trim(Fecdia) + '''' + ', '                                          
                      + '''' + %Trim(W_Pago) + '''' + ', '                                          
                      + '''' + %Trim(MEDA   ) + '''' +  ', '                                        
                      + %Trim(%editc(Seq:'X')) + ', '                                               
                      + '''' + %Trim(Adtend) + '''' +  ', '                                         
                      + '''' + %Trim(AdtndÑ) + '''' +  ')';                                         
                                                                                                    
               Exec Sql                                                                             
                 Prepare Cur2j From :SqlStr;                                                        
                                                                                                    
               Exec Sql                                                                             
                 Execute Cur2j;                                                                     
                                                                                                    
               Endif;                                                                               
                                                                                                    
           Endsr;                                                                                   
                                                                                                    
           Begsr srdiasMora;                                                                        
             SqlStr = *Blanks;                                                                      
                                                                                                    
             Sqlstr = 'Select Orden, Vr_Factura, Fecha_Vencimiento ' +                              
                 'From ' + %Trim(Librer) + '/' + %Trim(Archiv) +                                    
                 ' Where Tipo = ''Factura'' ' +                                                     
                 ' Group By Orden, Vr_factura, Fecha_Vencimiento ';                                 
                                                                                                    
            Exec Sql                                                                                
              Prepare C1 From :SqlStr;                                                              
                                                                                                    
            Exec Sql                                                                                
              Declare C1 Cursor For C1;                                                             
                                                                                                    
            Exec Sql                                                                                
              Open C1;                                                                              
                                                                                                    
            Exec Sql                                                                                
              Fetch C1 Into :W_Ord, :W_ValF, :W_FecV;                                               
                                                                                                    
            Dow SqlCode = *Zeros;                                                                   
                                                                                                    
              W_Ordt = W_Ord;                                                                       
              W_PorCal = W_ValF * .99;                                                              
                                                                                                    
              Sqlstr = 'Select (-1 * Vr_Pago), Fecha_Facturacion ' +                                
                  'From ' + %Trim(Librer) + '/' + %Trim(Archiv) +                                   
                  ' Where Tipo <> ''Factura'' And ' +                                               
                  'Orden = ' + %Trim(%Char(W_Ord)) +                                                
                  ' Order By Fecha_Facturacion ';                                                   
                                                                                                    
              Exec Sql                                                                              
                Prepare C3 From :SqlStr;                                                            
                                                                                                    
              Exec Sql                                                                              
                Declare C3 Cursor For C3;                                                           
                                                                                                    
              Exec Sql                                                                              
                Open C3;                                                                            
                                                                                                    
              Exec Sql                                                                              
                Fetch C3 Into :W_ValPago, :W_Fecf;                                                  
                                                                                                    
              Clear W_TotalPago;                                                                    
                                                                                                    
              Dow SqlCode = *Zeros;                                                                 
                W_TotalPago = W_totalPago + W_ValPago;                                              
                If W_TotalPago >= W_PorCal;                                                         
                  Monitor;                                                                          
                    FecDia = %Char(%Diff(%Date(W_FecF):%Date(W_FecV):*Days));                       
                  On-Error;                                                                         
                    FecDia = '';                                                                    
                  EndMon;                                                                           
                                                                                                    
                  If FecDia <> ' ';                                                                 
                    SqlStr = 'Update ' + %Trim(Librer) + '/' +                                      
                              %Trim(Archiv)+ ' Set DIAS_MORA = ' +                                  
                              '''' + %Trim(Fecdia) + '''' + ' Where ' +                             
                              'ORDEN = ' + %Trim(%editc(W_Ord:'Z')) + ' ' +                         
                              'And TIPO = ''Factura''';                                             
                                                                                                    
                    Exec Sql                                                                        
                      Execute Immediate :SqlStr;                                                    
                    Leave;                                                                          
                  EndIf;                                                                            
                EndIf;                                                                              
                Exec Sql                                                                            
                  Fetch C3 Into :W_ValPago, :W_Fecf;                                                
              EndDo;                                                                                
                                                                                                    
              Exec Sql                                                                              
                Close C3;                                                                           
                                                                                                    
              Exec Sql                                                                              
                Fetch C1 Into :W_Ord, :W_ValF, :W_FecV;                                             
                                                                                                    
            EndDo;                                                                                  
                                                                                                    
            Exec Sql                                                                                
              Close C1;                                                                             
                                                                                                    
           Endsr;                                                                                   
                                                                                                    
      /End-Free                                                                                     
