***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCC23R                                         *                         
     ** Descripciòn:  : Store procedure Informar sobre la solicitud       *                         
     ** Proyecto      : Estado de Cuenta                                  *                         
     ** Autor         : John William Palacio Personalsoft                 *                         
     ** Fecha Creaciòn: 01 de Abrir de 2016                               *                         
     **                                                                   *                         
     **********************************************************************                         
                                                                                                    
     ** Definición de Directivas de Compilación                                                     
     HDFTACTGRP(*NO)                                                                                
     HOption(*NoDebugIO)                                                                            
                                                                                                    
     **Definiciòn de Archivos de Trabajo                                                            
     FVteacrl1  if A e           k disk    Usropn                                                   
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DPciaR            s             10a   Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DUcia             s             10    Inz(*Blanks)                                             
     DTpar             s              1a   Inz(*Blanks)                                             
     D$COMA            C                   ''''                                                     
     DWcia             s              3a   Inz(*Blanks)                                             
     DWsql             S           2000A                                                            
     DI                S              1  0                                                          
     DWvalor           S            970A                                                            
     DWMensaje         S          20000A                                                            
     DWEmail           S             50A                                                            
     DWNumero          S             30  0                                                          
     DTced             s             30    Inz(*Blanks)                                             
     DTcns             s              9    Inz(*Blanks)                                             
     DW_User           S             10A   Inz(*User)                                               
     DW_Mensaje        S            500A   Inz(*User)                                               
     DW_Proce          S             10A                                                            
     DW_Tabla          S          20000A                                                            
     DW_Tabla1         S          20000A                                                            
     DW_Tabla2         S          20000A                                                            
                                                                                                    
     DPDeEmail         s            200a                                                            
     DPParaEmail       s            200a                                                            
     DPRemitenteM      s            200a                                                            
     DPTextEmail       s          10000                                                             
     DPcc              s          10000A                                                            
     DPcco             s          10000A                                                            
     DSoapr            s          32767A                                                            
     DPremitente       s            200A                                                            
     DPdestinatario    s            200A                                                            
     DPRemMostrar      s           1000A                                                            
     DPAsunto          s           1000A                                                            
     DPReplyto         s          10000A                                                            
     DPmensaje         s          32767A                                                            
     Dnombrepai        s             12                                                             
     DQpol1            s             17                                                             
     DQpol2            s             17                                                             
     DQpol3            s              5  0                                                          
     DQpol4            s              9  0                                                          
     DW_CEDTEM         s              9  0                                                          
     DPCODCHAR         s              9                                                             
     DWMLZIPC          s             10                                                             
     DPCedula          s              9  0                                                          
                                                                                                    
     ** Definición de Estructuras de Trabajo                                                        
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrTitulos        Pr                                                                           
     DPrObteValor      Pr                                                                           
     DPrValidar        Pr              n                                                            
                                                                                                    
     DFields         e Ds                  Extname(Restado:REGRES)                                  
     D                                     Prefix(Q)                                                
                                                                                                    
     Dcorreods         Ds                                                                           
     DCorreo                         80A                                                            
                                                                                                    
     DPrograma         Pr                  Extpgm('VOTREP00/VTSPAM0L')                              
     DTcia                            3                                                             
     DTpar                            1                                                             
                                                                                                    
     DConversionID     Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DPcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
     DPgmVTMCCC15L     Pr                  Extpgm('VOTREP00/VTMCCC15L')                             
     DPcia                            3                                                             
     DTZon                            3                                                             
     DTCed                            9                                                             
     DTInd                            1                                                             
                                                                                                    
     DCompaÑia         Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DUcia                           10                                                             
     Dkres                            2                                                             
     DkciaR                          10                                                             
                                                                                                    
                                                                                                    
     DEnviaCorreo      Pr                  Extpgm('VOTREP00/VTEPWB6R')                              
     DPcia                            3A                                                            
     DPres                            2A                                                            
     DPremitente                    200A                                                            
     DPdestinatario                 200A                                                            
     DPRemMostrar                  1000A                                                            
     DPAsunto                      1000A                                                            
     DPmensaje                    32767A                                                            
     DPReplyto                    10000A                                                            
     DPcc                         10000A                                                            
     DPcco                        10000A                                                            
     DSoapr                       32767A                                                            
                                                                                                    
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                           10a                                                            
     DPCod                           15  0                                                          
     DPZon                            3a                                                            
     DPCampa                          4A                                                            
     DPInd                            1A                                                            
     DPstatus                         1A                                                            
     DPdestatus                     120A                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                           10a                                                            
     DPCod                           15  0                                                          
     DPZon                            3a                                                            
     DPCampa                          4A                                                            
     DPInd                            1A                                                            
     DPstatus                         1A                                                            
     DPdestatus                     120A                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       //Se limpian las variables de salida                                                         
       Pstatus = *Blanks;                                                                           
       Pdestatus = *Blanks;                                                                         
       // Valida el Número de Parametros Enviados desde Java                                        
       If %Parms < 5;                                                                               
         Pstatus = '1';                                                                             
         Pdestatus = 'No se Enviaron los Parametros Requeridos';                                    
        *Inlr = *On;                                                                                
        //Return;                                                                                   
       Endif;                                                                                       
                                                                                                    
       If PCia = *Blanks;                                                                           
         Pstatus = '1';                                                                             
         Pdestatus = 'La Compañia No puede estar vacia';                                            
       Else;                                                                                        
         Ucia = Pcia;                                                                               
         CompaÑia(Ucia:Pres:PciaR);                                                                 
         If Pres = 'Si';                                                                            
          Tpar = 'A';                                                                               
          Wcia = %Trim(PciaR);                                                                      
          Programa(WCia:Tpar);                                                                      
                                                                                                    
          Clear Tcns;                                                                               
          TCed = %Trim(%Char(PCod));                                                                
          ConversionID(Wcia:Tced:Tcns);                                                             
          PCedula = %Int(Tcns);                                                                     
                                                                                                    
          // recupera zona                                                                          
          If PZon = *Blanks;                                                                        
                                                                                                    
             Clear WMLZIPC;                                                                         
             Exec Sql                                                                               
               Select MLZIPC Into :WMLZIPC                                                          
                 From LMLLST01                                                                      
                 Where MLCSTÑ  = :Pcedula                                                           
                 Fetch first 1 rows Only;                                                           
                                                                                                    
             Pzon = WMLZIPC;                                                                        
                                                                                                    
             If Pzon = ' ';                                                                         
                Pzon = '999';                                                                       
             Endif;                                                                                 
                                                                                                    
          Endif;                                                                                    
          Open Vteacrl1;                                                                            
          PrValidarCed();                                                                           
          Chain (Pcedula:PCampa) Vteacrl1;                                                          
          If Not %Found(Vteacrl1);                                                                  
            PrDatosComp();                                                                          
            PrDatosCred();                                                                          
            PrDatosPagA();                                                                          
            PrDatosEstC();                                                                          
            PrDatosRang();                                                                          
            PrDatosPorR();                                                                          
            PrPolitiApl();                                                                          
            AcrFes = %Date();                                                                       
            AcrCmp = PCampa;                                                                        
            AcrFec = %Date();                                                                       
            AcrHor = %Time();                                                                       
            AcrUsu = W_User;                                                                        
            Write RegAcr;                                                                           
            PrEnviaCorreo();                                                                        
            Pstatus = '0';                                                                          
            //Pdestatus = 'Ok. Continua Proceso';                                                   
            Pdestatus = 'Solicitud ha sido enviada satisfactoriamente';                             
          Else;                                                                                     
            Pstatus = '1';                                                                          
            Pdestatus = 'La solicitud ya fue enviada para esta campaña. ' +                         
                        'No olvides enviar la papeleria a la compañia' ;                            
          EndIf;                                                                                    
          Close Vteacrl1;                                                                           
                                                                                                    
          Tpar = 'E';                                                                               
          Programa(Wcia:Tpar);                                                                      
         Else;                                                                                      
           Pstatus = '1';                                                                           
           Pdestatus = 'La Compañia no Existe';                                                     
          *Inlr = *On;                                                                              
          //Return;                                                                                 
         Endif;                                                                                     
       EndIf;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de consultar los datos de la compradora  //                       
       //------------------------------------------------------------------//                       
     PPrDatosComp      B                                                                            
     DPrDatosComp      Pi                                                                           
      /Free                                                                                         
       Wsql = 'Select Nombre, TipComp, Clacomp, cupo ' +                                            
             'From Votrea' + %Trim(Wcia) + '/Vtwr' + %Trim(PInd) + %Trim(PZon) +                    
              ' Where TipoTra = ''Compradora''';                                                    
                                                                                                    
       Exec Sql                                                                                     
         Prepare C1 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C1 Scroll Cursor For C1;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C1;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C1 Into :QNombre, :QTipComp, :QClacomp, :QCupo;                                      
                                                                                                    
       If SqlCode = *Zeros;                                                                         
         Clear Tcns;                                                                                
         TCed = %Trim(%Char(PCod));                                                                 
         ConversionID(Wcia:Tced:Tcns);                                                              
         PCod = %Int(Tcns);                                                                         
         AcrCod = PCod;                                                                             
         AcrCed = %Int(Tced);                                                                       
         AcrNom = QNombre;                                                                          
         AcrZna = %Trim(PZon);                                                                      
         AcrTip = QTipComp;                                                                         
         AcrCla = QClacomp;                                                                         
         AcrCua = QCupo;                                                                            
       Endif;                                                                                       
       Exec Sql                                                                                     
         Close C1;                                                                                  
      /End-Free                                                                                     
     PPrDatosComp      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de consultar los datos de pago en credito//                       
       //------------------------------------------------------------------//                       
     PPrDatosCred      B                                                                            
     DPrDatosCred      Pi                                                                           
      /Free                                                                                         
       Wsql = 'Select NroPEd ' +                                                                    
              'From Votrea' + %Trim(Wcia) + '/Vtwr'+ %Trim(PInd) + %Trim(PZon) +                    
              ' Where TipoTra = ''PagoCredito''';                                                   
                                                                                                    
       Exec Sql                                                                                     
         Prepare C2 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C2 Scroll Cursor For C2;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C2;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C2 Into :QNroPEd;                                                                    
                                                                                                    
       If SqlCode = *Zeros;                                                                         
         AcrNpc = QNroPEd;                                                                          
       EndIf;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close C2;                                                                                  
                                                                                                    
                                                                                                    
      /End-Free                                                                                     
     PPrDatosCred      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de consultar los datos de pago anticipado//                       
       //------------------------------------------------------------------//                       
     PPrDatosPagA      B                                                                            
     DPrDatosPAgA      Pi                                                                           
      /Free                                                                                         
       Wsql = 'Select NroPEd ' +                                                                    
              'From Votrea' + %Trim(Wcia) + '/Vtwr'+ %Trim(PInd) + %Trim(PZon) +                    
              ' Where TipoTra = ''PagoAnticipado''';                                                
                                                                                                    
       Exec Sql                                                                                     
         Prepare C3 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C3 Scroll Cursor For C3;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C3;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C3 Into :QNroPEd;                                                                    
                                                                                                    
       If sqlCode = *Zeros;                                                                         
         AcrNpr = QNroPEd;                                                                          
       EndIf;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close C3;                                                                                  
                                                                                                    
                                                                                                    
      /End-Free                                                                                     
     PPrDatosPagA      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de consultar los datos del estudio de cre//                       
       //------------------------------------------------------------------//                       
     PPrDatosEstC      B                                                                            
     DPrDatosEstC      Pi                                                                           
      /Free                                                                                         
       Wsql = 'Select Aproba, Negado, Condic, Cupoas, Observ, NpeDco ' +                            
              'From Votrea' + %Trim(Wcia) + '/Vtwr'+ %Trim(PInd) + %Trim(PZon) +                    
              ' Where TipoTra = ''EstudioCredito''';                                                
                                                                                                    
       Exec Sql                                                                                     
         Prepare C4 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C4 Scroll Cursor For C4;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C4;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C4 Into :QAproba, :QNegado, :QCondic, :QCupoas,                                      
                       :QObserv, :QNpeDco;                                                          
                                                                                                    
       If SqlCode = *Zeros;                                                                         
         AcrApr = QAproba;                                                                          
         AcrNeg = QNegado;                                                                          
         AcrCon = QCondic;                                                                          
         AcrCup = QCupoas;                                                                          
         AcrCom = QObserv;                                                                          
         AcrNpa = QNpeDco;                                                                          
       EndIf;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close C4;                                                                                  
                                                                                                    
      /End-Free                                                                                     
     PPrDatosEstC      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de consultar los datos de los rangos     //                       
       //------------------------------------------------------------------//                       
     PPrDatosRang      B                                                                            
     DPrDatosRang      Pi                                                                           
      /Free                                                                                         
       Wsql = 'Select Rango1, Rango2, Rango3, Rango4, Rango5, Rango6, ' +                           
              'Rango7 , Rango8, Rango9 ' +                                                          
              'From Votrea' + %Trim(Wcia) + '/Vtwr'+ %Trim(PInd) + %Trim(PZon) +                    
              ' Where TipoTra = ''CarteraEdades''';                                                 
                                                                                                    
       Exec Sql                                                                                     
         Prepare C5 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C5 Scroll Cursor For C5;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C5;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C5 Into :QRango1, :QRango2, :QRango3, :QRango4,                                      
                       :QRango5, :QRango6, :QRango7, :QRango8,                                      
                       :QRango9;                                                                    
                                                                                                    
       If sqlCode = *Zeros;                                                                         
         AcrRa1 = QRango1;                                                                          
         AcrRa2 = QRango2;                                                                          
         AcrRa3 = QRango3;                                                                          
         AcrRa4 = QRango4;                                                                          
         AcrRa5 = QRango5;                                                                          
         AcrRa6 = QRango6;                                                                          
         AcrRa7 = QRango7;                                                                          
         AcrRa8 = QRango8;                                                                          
         AcrRa9 = QRango9;                                                                          
       EndIf;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close C5;                                                                                  
      /End-Free                                                                                     
     PPrDatosRang      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de consultar los datos de los porcentajes//                       
       //------------------------------------------------------------------//                       
     PPrDatosPorR      B                                                                            
     DPrDatosPorR      Pi                                                                           
      /Free                                                                                         
       Wsql = 'Select Porra1, Porra2, Porra3, Porra4, Porra5, ' +                                   
              'Porra6, Porra7, Porra8, Porra9 ' +                                                   
              'From Votrea' + %Trim(Wcia) + '/Vtwr'+ %Trim(PInd) + %Trim(PZon) +                    
              ' Where TipoTra = ''%CarteraEdades''';                                                
                                                                                                    
       Exec Sql                                                                                     
         Prepare C6 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C6 Scroll Cursor For C6;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C6;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C6 Into :QPorra1, :QPorra2, :QPorra3, :QPorra4, :QPorra5,                            
                       :QPorra6, :QPorra7, :QPorra8, :QPorra9;                                      
                                                                                                    
       If SqlCode = *Zeros;                                                                         
         AcrRg1 = QPorra1;                                                                          
         AcrRg2 = QPorra2;                                                                          
         AcrRg3 = QPorra3;                                                                          
         AcrRg4 = QPorra4;                                                                          
         AcrRg5 = QPorra5;                                                                          
         AcrRg6 = QPorra6;                                                                          
         AcrRg7 = QPorra7;                                                                          
         AcrRg8 = QPorra8;                                                                          
         AcrRg9 = QPorra9;                                                                          
       EndIf;                                                                                       
       Exec Sql                                                                                     
         Close C6;                                                                                  
      /End-Free                                                                                     
     PPrDatosPorR      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de consultar los datos de los porcentajes//                       
       //------------------------------------------------------------------//                       
     PPrPolitiApl      B                                                                            
     DPrPolitiApl      Pi                                                                           
      /Free                                                                                         
       Wsql = 'Select Rango1 || '' a '' || Rango2, ' +                                              
              'Trim(Char(Integer(Porra1))) || ''% a ''' +                                           
              '|| Trim(Char(Integer(Porra2))) ||''%'', ' +                                          
              ' Rango3, Cupo ' +                                                                    
              'From Votrea' + %Trim(Wcia) + '/Vtwr'+ %Trim(PInd) + %Trim(PZon) +                    
              ' Where TipoTra Like ''%NuevaPol%''';                                                 
                                                                                                    
       Exec Sql                                                                                     
         Prepare C9 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C9 Scroll Cursor For C9;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C9;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C9 Into :QPol1, :QPol2, :Qpol3, :QPol4;                                              
                                                                                                    
       Exec Sql                                                                                     
         Close C9;                                                                                  
      /End-Free                                                                                     
     PPrPolitiApl      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de enviar el correo                      //                       
       //------------------------------------------------------------------//                       
     PPrEnviaCorreo    B                                                                            
     DPrEnviaCorreo    Pi                                                                           
      /Free                                                                                         
       Clear Pdestinatario;                                                                         
                                                                                                    
       W_Proce = 'EC' + %Trim(WCIa) + %Trim(PZon);                                                  
       //Recupera Email a enviar                                                                    
       Wsql = 'Select CuvMal ' +                                                                    
              'From Votrea00/Vtmcuvf ' +                                                            
              'Where CuvPro = ' + $Coma + %Trim(W_Proce) + $Coma +                                  
              ' And CuvEnv = ''S''';                                                                
                                                                                                    
       Exec Sql                                                                                     
         Prepare C7 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C7 Scroll Cursor For C7;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C7;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C7 Into :correods;                                                                   
                                                                                                    
        Dow SqlCod <> 100 And Sqlcod >= *Zeros;                                                     
          Pdestinatario = %Trim(Pdestinatario) + %Trim(Correo) + '|';                               
          Exec Sql                                                                                  
            Fetch C7 Into :correods;                                                                
       EndDo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close C7;                                                                                  
                                                                                                    
       Wsql = 'Select CasMsg ' +                                                                    
              'From Votrea00/Vtmcasf ' +                                                            
              'Where CasPro = ' + $Coma + %Trim(W_Proce) + $Coma;                                   
                                                                                                    
       Exec Sql                                                                                     
         Prepare C8 From :Wsql;                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Declare C8 Scroll Cursor For C8;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C8;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C8 Into :W_Mensaje;                                                                  
                                                                                                    
       // Valida que como minimo exista un destinatario                                             
       If Pdestinatario <>  *Blanks;                                                                
         // Asigna compañia                                                                         
         Pcia = WCia;                                                                               
                                                                                                    
         // Asigna Respuesta                                                                        
         Pres= *Blanks;                                                                             
                                                                                                    
         // Asigna Remitente                                                                        
         Premitente = 'CarteraLeonisa@leonisa.com';                                                 
                                                                                                    
         // Asigna Descripcion mensaje                                                              
         PRemMostrar = 'Enviar Solicitud';                                                          
                                                                                                    
         // Asigna asunto                                                                           
         Pasunto = 'Solicitud de Estudio Reincorporación AB ' + %Trim(Tced) +                       
                   ' Zona ' + %Trim(PZon);                                                          
         PrArmarTabla();                                                                            
                                                                                                    
         // Asigna asunto                                                                           
         Pmensaje   = %Trim(W_Mensaje) +                                                            
                      '<Br><Br> '  +                                                                
                      %Trim(W_Tabla) + '<Br><Br> '  +                                               
                      %Trim(W_Tabla1) + '<Br><Br> '  +                                              
                      %Trim(W_Tabla2);                                                              
                                                                                                    
         PReplyto   = *Blanks;                                                                      
         Pcc        = *Blanks;                                                                      
         Pcco       = *Blanks;                                                                      
         Soapr      = *Blanks;                                                                      
          EnviaCorreo(Pcia           :                                                              
                     Pres            :                                                              
                     Premitente      :                                                              
                     Pdestinatario   :                                                              
                     PRemMostrar     :                                                              
                     PAsunto         :                                                              
                     Pmensaje        :                                                              
                     PReplyto        :                                                              
                     Pcc             :                                                              
                     Pcco            :                                                              
                     Soapr            );                                                            
                                                                                                    
       Endif; // Fin validación destinatario                                                        
                                                                                                    
       Exec Sql                                                                                     
         Close C8;                                                                                  
      /End-Free                                                                                     
     PPrEnviaCorreo    E                                                                            
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de enviar el correo                      //                       
       //------------------------------------------------------------------//                       
     PPrarmarTabla     B                                                                            
     DPrArmarTabla     Pi                                                                           
      /Free                                                                                         
       W_Tabla = '<table border="1" cellpadding="0" cellspacing="0"' +                              
                 'width="100%"> <tbody> ' +                                                         
                 '<tr style="background-color'+                                                     
                 ':#1f3251; color:white;'+                                                          
                 'text-align:center; height: 25px;">' +                                             
                 '<td>ZONA</td>' +                                                                  
                 '<td>CEDULA</td> '+                                                                
                 '<td>NOMBRE</td>' +                                                                
                 '<td>TIPO DE COMPRADORA </td>' +                                                   
                 '<td>CUPO</td></tr>' +                                                             
                 '<tr style="text-align:center;height: 25px;">'+                                    
                 '<td>' + %Trim(PZon) + '</td>' +                                                   
                 '<td>' + %Trim(Tced) + '</td>' +                                                   
                 '<td>' + %Trim(QNombre) + '</td>' +                                                
                 '<td>' + %Trim(QTipComp) + '</td>' +                                               
                 '<td>' + %Trim(%Char(QCupo)) + '</td></tr></table>';                               
       W_Tabla = %ScanRpl('"':'''':W_Tabla);                                                        
                                                                                                    
       W_Tabla1 = '<table border="1" cellpadding="0" cellspacing="0"' +                             
                 'width="100%"> <tbody> ' +                                                         
                 '<tr style="background-color'+                                                     
                 ':#1f3251; color:white;'+                                                          
                 'text-align:center; height: 25px;">' +                                             
                 '<td>APROBADO</td>' +                                                              
                 '<td>NEGADO</td> '+                                                                
                 '<td>CONDICION</td>' +                                                             
                 '<td>NRO PEDIDOS PAGO ANTICIPADO </td>' +                                          
                 '<td>CUPO ASIGNADO</td>' +                                                         
                 '<td>COMENTARIO</td></tr>' +                                                       
                 '<tr style="text-align:center;height: 25px;">'+                                    
                 '<td>' + %Trim(QAproba) + '</td>' +                                                
                 '<td>' + %Trim(QNegado) + '</td>' +                                                
                 '<td>' + %Trim(QCondic) + '</td>' +                                                
                 '<td>' + %Trim(%Char(QNpeDco)) + '</td>' +                                         
                 '<td>' + %Trim(%Char(QCupoas)) + '</td>' +                                         
                 '<td>' + %Trim(QObserv) + '</td></tr></table>';                                    
       W_Tabla1 = %ScanRpl('"':'''':W_Tabla1);                                                      
                                                                                                    
       W_Tabla2 = '<table border="1" cellpadding="0" cellspacing="0"' +                             
                 'width="100%"> <tbody> ' +                                                         
                 '<tr style="background-color'+                                                     
                 ':#1f3251; color:white;'+                                                          
                 'text-align:center; height: 25px;">' +                                             
                 '<td># PEDIDOS EN CR</td>' +                                                       
                 '<td># PEDIDOS PAGADOS A TIEMPO</td> '+                                            
                 '<td>PEDIDOS EFECTIVOS PAGADOS A TIEMPO</td>' +                                    
                 '<td>CUPO</td></tr>' +                                                             
                 '<tr style="text-align:center;height: 25px;">'+                                    
                 '<td>' + %Trim(Qpol1) + '</td>' +                                                  
                 '<td>' + %Trim(Qpol2) + '</td>' +                                                  
                 '<td>' + %Trim(%Char(QPol3)) + '</td>' +                                           
                 '<td>' + %Trim(%Char(QPol4)) + '</td></tr></table>';                               
       W_Tabla2 = %ScanRpl('"':'''':W_Tabla2);                                                      
      /End-Free                                                                                     
     PPrArmarTabla     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de enviar el correo                      //                       
       //------------------------------------------------------------------//                       
     PPrValidarCed     B                                                                            
     DPrValidarCed     Pi                                                                           
      /Free                                                                                         
       Wsql = 'Select Cedula ' +                                                                    
              'From Votrea' + %Trim(Wcia) + '/Vtwr'+ %Trim(PInd) + %Trim(PZon) +                    
              ' Where TipoTra = ''Compradora''';                                                    
                                                                                                    
       Exec Sql                                                                                     
         Prepare C10 From :Wsql;                                                                    
                                                                                                    
       Exec Sql                                                                                     
         Declare C10 Scroll Cursor For C10;                                                         
                                                                                                    
       Exec Sql                                                                                     
         Open C10;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
         fetch C10 Into :W_CedTem;                                                                  
                                                                                                    
       Exec Sql                                                                                     
         Close C10;                                                                                 
                                                                                                    
       If W_CedTem <> PCod ;                                                                        
         PCodChar = %Char(PCedula);                                                                 
         PgmVTMCCC15L(Wcia:PZon:PCodChar:PInd);                                                     
       EndIf;                                                                                       
                                                                                                    
                                                                                                    
                                                                                                    
      /End-Free                                                                                     
     PPrValidarCed     E                                                                            
