***************************************************************************                         
     ** Programa      : VTPSGBSR                                          *                         
     ** Descripción:  : Valida si la Zona Ingreso la Información del Ppto *                         
     ** Proyecto      : Plan Guías                                        *                         
     ** Autor         : (JOrtega)                                         *                         
     ** Fecha Creación: 08 de Julio de 2016                               *                         
     **********************************************************************                         
                                                                                                    
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definición de Variables de Trabajo                                                          
     FVtwrnpf   Uf a e           k Disk                                                             
                                                                                                    
     ** Definición de Variables de Trabajo                                                          
     DCmd              s           1500                                                             
     DLong             s             15p 5                                                          
     DWnro             s              9  0 Inz(*Zeros)                                              
     DIzona            s              3a   Inz(*Blanks)                                             
     DIcaso            s              7a   Inz(*Blanks)                                             
     DIcedula          s             30a   Inz(*Blanks)                                             
     DIfecha           s             10a   Inz(*Blanks)                                             
     DOrespuesta       s              1a   Inz(*Blanks)                                             
     DEmail            s            200a   Inz(*Blanks)                                             
     Dw_EMAIL          s            200a   Inz(*Blanks)                                             
     DRnpciaW          s              3a   Inz(*Blanks)                                             
     DRnpzonW          s              3a   Inz(*Blanks)                                             
     DRnpcmpW          s              4a   Inz(*Blanks)                                             
     DSoapr            s          32767a   Inz(*Blanks)                                             
     DPremitente       s            200a   Inz(*Blanks)                                             
     DPdestinatario    s            200a   Inz(*Blanks)                                             
     DPRemMostrar      s           1000a   Inz(*Blanks)                                             
     DPAsunto          s           1000a   Inz(*Blanks)                                             
     DPReplyto         s          10000a   Inz(*Blanks)                                             
     DPmensaje         s          32767a   Inz(*Blanks)                                             
     DPcc              s          10000a   Inz(*Blanks)                                             
     DPcco             s          10000a   Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DRespuesta        s              8a   Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DRegion           s             10a   Inz(*Blanks)                                             
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
     DPrPptoZon        Pr                                                                           
     DPrReproZon       Pr                                                                           
     DComandoSbm       Pr                  Extpgm('QCMDEXC')                                        
     D                             1500                                                             
     D                               15  5                                                          
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DP_Cia                           3a                                                            
     DP_Med                           4a                                                            
     DP_Zon                           3a                                                            
     DP_Ind                           1a                                                            
     DP_Acc                           1a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DP_Cia                           3a                                                            
     DP_Med                           4a                                                            
     DP_Zon                           3a                                                            
     DP_Ind                           1a                                                            
     DP_Acc                           1a                                                            
                                                                                                    
     DPgmNewPlanG      Pr                  Extpgm('VOTREP00/VTMPGB0R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcaso                           7a                                                            
     DIcedula                        30a                                                            
     DIfecha                         10a                                                            
     DOrespuesta                      1a                                                            
                                                                                                    
     DPrEnviaCorreo    Pr                                                                           
     DValidarEmail     Pr                  Extpgm('VOTREP00/VTMGOBTR')                              
     DPrtStatus                       8a                                                            
     DPemail                        200a                                                            
     DPrComercial      Pr          1000a                                                            
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DEnviaCorreo      Pr                  Extpgm('VOTREP00/VTEPWB3R')                              
     DPcia                            3a                                                            
     DPres                            2a                                                            
     DPremitente                    200a                                                            
     DPdestinatario                 200a                                                            
     DPRemMostrar                  1000a                                                            
     DPAsunto                      1000a                                                            
     DPmensaje                    32767a                                                            
     DPReplyto                    10000a                                                            
     DPcc                         10000a                                                            
     DPcco                        10000a                                                            
     DSoapr                       32767a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       P_Ind = 'N';                                                                                 
       Icedula = *Blanks;                                                                           
       If P_Acc = 'V';                                                                              
        Clear Orespuesta;                                                                           
        PgmNewPlanG(P_Cia:P_Zon:Icaso:Icedula:Ifecha:Orespuesta);                                   
        If Orespuesta = 'S';                                                                        
         PrPptoZon();                                                                               
        Endif;                                                                                      
       Endif;                                                                                       
       If P_Acc = 'R';                                                                              
        PrReproZon();                                                                               
       Endif;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Valida si la Zona Ingreso el Presupuesto       //                       
       //------------------------------------------------------------------//                       
     PPrPptoZon        B                                                                            
     DPrPptoZon        Pi                                                                           
      /Free                                                                                         
       Clear Wnro;                                                                                  
       Exec Sql                                                                                     
        Select Count(0) Into :Wnro                                                                  
        From Votrea00/Vtbptofs                                                                      
        Where Ciapts = :P_Cia And                                                                   
              Campts = :P_Med And                                                                   
              Zonpts = :P_Zon;                                                                      
                                                                                                    
       If Wnro > *Zeros;                                                                            
        P_Ind = 'S';                                                                                
       Else;                                                                                        
        If PrVtmrzgf() = *On;                                                                       
         Chain (P_cia:P_Med:P_Zon) Vtwrnpf;                                                         
         If Not %Found(Vtwrnpf);                                                                    
          Rnpcia = P_Cia;                                                                           
          Rnpzon = P_Zon;                                                                           
          Rnpcmp = P_Med;                                                                           
          Rnpidp = *Blanks;                                                                         
          Write Regrnp;                                                                             
         Else;                                                                                      
          Rnpidp = *Blanks;                                                                         
          Update Regrnp;                                                                            
         Endif;                                                                                     
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
      /End-Free                                                                                     
     PPrPptoZon        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Reprocesa las Zonas que Faltan por Pago        //                       
       //------------------------------------------------------------------//                       
     PPrReproZon       B                                                                            
     DPrReproZon       Pi                                                                           
     DInro             s              3  0 Inz(*Zeros)                                              
      /Free                                                                                         
       Clear Inro;                                                                                  
       Exec Sql                                                                                     
        Select Count(0) Into :Inro                                                                  
        From Votrea00/Vtwrnpf                                                                       
        Where Rnpcia = :P_Cia And                                                                   
              Rnpcmp = :P_Med And                                                                   
              Rnpzon = :P_Zon;                                                                      
                                                                                                    
       If Inro > 0;                                                                                 
       RnpciaW = *Blanks;                                                                           
       RnpzonW = *Blanks;                                                                           
       RnpcmpW = *Blanks;                                                                           
       Exec Sql                                                                                     
        Declare Cur0 Cursor For                                                                     
        Select Rnpcia, Rnpzon, Rnpcmp                                                               
        From Votrea00/Vtwrnpf                                                                       
        Where Rnpidp = ' ';                                                                         
                                                                                                    
       Exec Sql                                                                                     
        Open Cur0;                                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cur0 Into :RnpciaW, :RnpzonW, :RnpcmpW;                                               
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
        Orespuesta = *Blanks;                                                                       
        Icedula = *Blanks;                                                                          
        Icaso = *Blanks;                                                                            
        PgmNewPlanG(RnpciaW:RnpzonW:Icaso:Icedula:Ifecha:Orespuesta);                               
        If Orespuesta = 'S';                                                                        
         Wnro = *Zeros;                                                                             
         Exec Sql                                                                                   
          Select Count(0) Into :Wnro                                                                
          From Votrea00/Vtbptofs                                                                    
          Where Ciapts = :RnpciaW And                                                               
                Campts = :RnpcmpW And                                                               
                Zonpts = :RnpzonW;                                                                  
                                                                                                    
         If Wnro > *Zeros;                                                                          
          Chain (RnpciaW:RnpcmpW:RnpzonW) Vtwrnpf;                                                  
          If %Found(Vtwrnpf);                                                                       
           Rnpidp = 'P';                                                                            
           Update Regrnp;                                                                           
           Cmd = 'SBMJOB CMD(CALL PGM(VOTREP00/VTPCGB0L)' +                                         
                 ' PARM(''' + %Trim((RnpciaW)) + ''' +                                              
                        ''' + %Trim((RnpcmpW)) + ''' +                                              
                        ''' + %Trim((RnpzonW)) + '''))' +                                           
                        ' JOB(NEWPLGUIAS) JOBQ(GENERP00/PRCGUIAS)';                                 
           Long=%Len(%Trim(Cmd));                                                                   
           Callp(E) ComandoSbm(Cmd:Long);                                                           
          Endif;                                                                                    
         Else;                                                                                      
          If Orespuesta = 'S';                                                                      
           PrEnviaCorreo();                                                                         
          Endif;                                                                                    
         Endif;                                                                                     
        Endif;                                                                                      
                                                                                                    
        If Orespuesta = 'N';                                                                        
         Chain (P_cia:P_Med:P_Zon) Vtwrnpf;                                                         
         If %Found(Vtwrnpf);                                                                        
          Delete Regrnp;                                                                            
         Endif;                                                                                     
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch Cur0 Into :RnpciaW, :RnpzonW, :RnpcmpW;                                              
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cur0;                                                                                 
                                                                                                    
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrReproZon       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Encargado de Enviar el Correo                      //                       
       //------------------------------------------------------------------//                       
     PPrEnviaCorreo    B                                                                            
     DPrEnviaCorreo    Pi                                                                           
     ** Definiciòn de Variables de Trabajo                                                          
     DWlider           s           1000a   Inz(*Blanks)                                             
     DWregional        s           1000a   Inz(*Blanks)                                             
     DWcomercial       s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
       Clear Pdestinatario;                                                                         
       Wlider = PrLider();                                                                          
       Wregional = PrRegional();                                                                    
       Wcomercial = PrComercial();                                                                  
       Pdestinatario = %ScanRpl(',':'|':Wcomercial);                                                
                                                                                                    
       // Email Lider                                                                               
       If %Len(%Trim(Wlider)) > *Zeros;                                                             
        Pdestinatario = %Trim(Wlider);                                                              
       Endif;                                                                                       
        // Email Regional                                                                           
        If %Len(%Trim(Wregional)) > *Zeros;                                                         
         If %Len(%Trim(Pdestinatario)) <= *Zeros;                                                   
          Pdestinatario = %Trim(Wregional);                                                         
         Else;                                                                                      
          Pdestinatario = %Trim(Pdestinatario) + '|' + %Trim(Wregional);                            
         Endif;                                                                                     
        Endif;                                                                                      
       // Email Area Comercial                                                                      
       If %Len(%Trim(Wcomercial)) > *Zeros;                                                         
        If %Len(%Trim(Pdestinatario)) <= *Zeros;                                                    
         Pdestinatario = %Trim(Wcomercial);                                                         
        Else;                                                                                       
         Pdestinatario = %Trim(Pdestinatario) + '|' + %Trim(Wcomercial);                            
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       // Valida que como minimo exista un destinatario                                             
       If Pdestinatario <>  *Blanks;                                                                
                                                                                                    
         // Asigna Respuesta                                                                        
         Pres= *Blanks;                                                                             
                                                                                                    
         // Asigna Remitente                                                                        
         Premitente = 'servicioalcliente@leonisa.com';                                              
                                                                                                    
         // Asigna Descripcion mensaje                                                              
         PRemMostrar = 'Descuentos no Aplicados por no Tener Presupuesto ' +                        
                       'la Zona(' + P_Zon + ') en ' + %Trim(Xpais);                                 
                                                                                                    
         // Asigna asunto                                                                           
         Pasunto = %Trim(PRemMostrar);                                                              
                                                                                                    
         // Asigna asunto                                                                           
         Pmensaje   = 'Hola... <br><br>' +                                                          
                      'Es para Informarte que la zona ' + P_Zon +                                   
                      ' no se le generaron los ' +                                                  
                      'descuentos de la campaña ' + P_Med + ' porque ' +                            
               'la zona no tiene presupuestos asignados para esa ' +                                
                      'campaña..<br>Por favor verificar esta información en ' +                     
                      'el aplicativo de Presupuestos..';                                            
                                                                                                    
         PReplyto   = *Blanks;                                                                      
         Pcc        = *Blanks;                                                                      
         Pcco       = *Blanks;                                                                      
         Soapr      = *Blanks;                                                                      
                                                                                                    
         If P_Zon <> *Blanks;                                                                       
          EnviaCorreo(P_Cia          :                                                              
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
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrEnviaCorreo    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email del Area Comercial           //                       
       //------------------------------------------------------------------//                       
     PPrComercial      B                                                                            
     DPrComercial      Pi          1000a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DEmail            s            200a   Inz(*Blanks)                                             
     DWcomercial       s           1000a   Inz(*Blanks)                                             
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DNro              s              3  0 Inz(*Zeros)                                              
     DComa             s              1a   Inz(',')                                                 
      /Free                                                                                         
       Xpais = *Blanks;                                                                             
       PgmPais(P_cia:Xpais);                                                                        
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Seceml ' +                                                                  
                'From Votrea00/Vtmsecf ' +                                                          
                'Where Trim(Seccia) = ' + '''' + P_cia + '''' + ' And ' +                           
                'Trim(Secdiv) = ' + '''' + %Trim(Xpais) + '''' + ' And ' +                          
                'Secest = '' ''';                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur3 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor3 Cursor For Cur3;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor3;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor3 Into :Email;                                                                  
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
        Respuesta = *Blanks;                                                                        
        w_Email = Email;                                                                            
        ValidarEmail(Respuesta:w_Email);                                                            
        If %Trim(Respuesta) <> 'Invalido';                                                          
         Nro += 1;                                                                                  
         If Nro > 1;                                                                                
          Coma = ',';                                                                               
         Else;                                                                                      
          Coma = *Blanks;                                                                           
         Endif;                                                                                     
         WComercial = %Trim(WComercial) + %Trim(Coma) + %Trim(Email);                               
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch Cursor3 Into :Email;                                                                 
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor3;                                                                              
                                                                                                    
       Return WComercial;                                                                           
      /End-Free                                                                                     
     PPrComercial      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta Campoaña Inicio Pagos                 //                       
       //------------------------------------------------------------------//                       
     PPrVtmrzgf        B                                                                            
     DPrVtmrzgf        Pi              n                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DPsw              s               n   Inz(*Off)                                                
     DOcampa           s              4a   Inz(' ')                                                 
     DRzgfic           s             10a   Inz(' ')                                                 
     DRzgffc           s             10a   Inz(' ')                                                 
       Exec Sql                                                                                     
        Select Rzgfic, Rzgffc Into :Rzgfic, :Rzgffc                                                 
        From Votrea00/Vtmrzgf                                                                       
        Where Rzgest = ' ' And                                                                      
              Rzgcia = :P_Cia And                                                                   
              Rzgzna = :P_Zon;                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Select Fcfcmp Into :Ocampa                                                                  
        From Vpcopa00/Vtcmfcf                                                                       
        Where Fcfcia = :P_Cia And                                                                   
              Fcffin >= :Rzgfic                                                                     
        Order By Fcfcmp Asc                                                                         
        Fetch First 1 Row Only;                                                                     
                                                                                                    
       If P_Med > Ocampa;                                                                           
        Psw = *On;                                                                                  
       Endif;                                                                                       
                                                                                                    
       Return Psw;                                                                                  
      /End-Free                                                                                     
     PPrVtmrzgf        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email de la Líder                  //                       
       //------------------------------------------------------------------//                       
     PPrLider          B                                                                            
     DPrLider          Pi          1000a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DEmail            s            200a   Inz(*Blanks)                                             
     DWlider           s           1000a   Inz(*Blanks)                                             
     DSqlStr           s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
                                                                                                    
       Clear Region;                                                                                
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Daleml, Sminf2 ' +                                                          
                'From Vpcopa00/Vtmdalf Inner Join Opf' + P_cia +                                    
                '/Pslman On Trim(Sminf1) = Trim(Dalced) ' +                                         
                'Where Trim(Smslsp) = ' + '''' + P_Zon + '''' + ' And ' +                           
                'Smdele = '' ''' + ' ' +                                                            
                'Fetch First 1 Row Only';                                                           
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1 Cursor For Cur1;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1 Into :Email, :Region;                                                         
                                                                                                    
       If Sqlcod = 100 Or Sqlcod < *Zeros;                                                          
        Email = *Blanks;                                                                            
        Region = *Blanks;                                                                           
       Endif;                                                                                       
                                                                                                    
       Respuesta = *Blanks;                                                                         
       w_Email = Email;                                                                             
       ValidarEmail(Respuesta:w_Email);                                                             
       If %Trim(Respuesta) = 'Invalido';                                                            
        Wlider = *Blanks;                                                                           
       Else;                                                                                        
        Wlider = Email;                                                                             
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1;                                                                              
                                                                                                    
       Return Wlider;                                                                               
      /End-Free                                                                                     
     PPrLider          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email del Regional                 //                       
       //------------------------------------------------------------------//                       
     PPrRegional       B                                                                            
     DPrRegional       Pi          1000a                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DEmail            s            200a   Inz(*Blanks)                                             
     DWregional        s           1000a   Inz(*Blanks)                                             
     DSqlStr           s           1000a   Inz(*Blanks)                                             
      /Free                                                                                         
                                                                                                    
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Divemd ' +                                                                  
                'From Votrea00/Vtmdivf ' +                                                          
                'Where Trim(Divpai) = ' + '''' + P_cia + '''' + ' And ' +                           
                'Divest = '' ''' + ' And Divdiv =' + '''' + %Trim(Region)                           
                + ''' ' + ' ' +                                                                     
                'Fetch First 1 Row Only';                                                           
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur2 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor2 Cursor For Cur2;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor2;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor2 Into :Email;                                                                  
                                                                                                    
       If Sqlcod = 100 Or Sqlcod < *Zeros;                                                          
        Email = *Blanks;                                                                            
       Endif;                                                                                       
                                                                                                    
       Respuesta = *Blanks;                                                                         
       w_Email = Email;                                                                             
       ValidarEmail(Respuesta:w_Email);                                                             
       If %Trim(Respuesta) = 'Invalido';                                                            
        Wregional = *Blanks;                                                                        
       Else;                                                                                        
        Wregional = Email;                                                                          
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor2;                                                                              
                                                                                                    
       Return Wregional;                                                                            
      /End-Free                                                                                     
     PPrRegional       E                                                                            
