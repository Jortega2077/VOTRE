***************************************************************************                         
     ** Programa      : VTPSGB2R                                          *                         
     ** Descripción:  : Envio de Email Guías sin Enviar el RUT            *                         
     ** Proyecto      : Nuevo Plan de Guías                               *                         
     ** Autor         : (JOrtega)                                         *                         
     ** Fecha Creación: 17 de Marzo de 2016                               *                         
     **********************************************************************                         
                                                                                                    
      ** Definición de Directivas de Compilación                                                    
       Ctl-Opt Dftactgrp(*no) Bnddir('QC2LE');                                                      
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
       Dcl-s String Char(1000) Inz(*Blanks);                                                        
       Dcl-s Wpais Char(3) Inz(*Blanks);                                                            
       Dcl-s Cedula Packed(15:0) Inz(*Zeros);                                                       
       Dcl-s NroGuias Packed(15:0) Inz(*Zeros);                                                     
       Dcl-s IcedCor Char(9) Inz(*Blanks);                                                          
       Dcl-s WfechaI Char(10) Inz(*Blanks);                                                         
       Dcl-s Wlider Char(1000) Inz(*Blanks);                                                        
       Dcl-s Wregional Char(1000) Inz(*Blanks);                                                     
       Dcl-s Wcomercial Char(1000) Inz(*Blanks);                                                    
       Dcl-s w_Email Char(200) Inz(*Blanks);                                                        
       Dcl-s Respuesta Char(8) Inz(*Blanks);                                                        
       Dcl-s Region Char(10) Inz(*Blanks);                                                          
       Dcl-s Wnombre Char(50) Inz(*Blanks);                                                         
       Dcl-s Wzona Char(3) Inz(*Blanks);                                                            
       Dcl-s CmpIng Char(4) Inz(*Blanks);                                                           
       Dcl-s IcedLar Char(30) Inz(*Blanks);                                                         
       Dcl-s NroPd Packed(3:0) Inz(*Zeros);                                                         
       Dcl-s FecUefe Char(10) Inz(*Blanks);                                                         
       Dcl-s NroC Packed(3:0) Inz(*Zeros);                                                          
       Dcl-s PCamE Char(4) Inz(*Blanks);                                                            
       Dcl-s PCamR Char(4) Inz(*Blanks);                                                            
       Dcl-s POper Char(1) Inz(*Blanks);                                                            
       Dcl-s Pnume Char(3) Inz(*Blanks);                                                            
       Dcl-s Hrol Char(2) Inz(*Blanks);                                                             
       Dcl-s Hfec Char(10) Inz(*Blanks);                                                            
       Dcl-s Htexto Char(200) Inz(*Blanks);                                                         
       Dcl-s HtextoC Char(200) Inz(*Blanks);                                                        
       Dcl-s Wtexto Char(200) Inz(*Blanks);                                                         
       Dcl-s WtextoC Char(200) Inz(*Blanks);                                                        
                                                                                                    
      ** Definición de Procedimientos Parametros de Entrada                                         
       Dcl-Pr PrGuiasActivas End-Pr;                                                                
       Dcl-Pr PrGuiasActivasPorZona End-Pr;                                                         
       Dcl-Pr PrVerificarRut End-Pr;                                                                
       Dcl-Pr PrEnviarEmail End-Pr;                                                                 
       Dcl-Pr PrInformacionCp End-Pr;                                                               
       Dcl-Pr PrLiderDet Char(1000) End-Pr;                                                         
       Dcl-Pr PrRegional Char(1000) End-Pr;                                                         
       Dcl-Pr PrComercial Char(1000) End-Pr;                                                        
       Dcl-Pr PgmNewPlanG Extpgm('VTMPGB0R');                                                       
        Icia Char(3);                                                                               
        Izona Char(3);                                                                              
        Icaso Char(7);                                                                              
        Icedula Char(30);                                                                           
        Ifecha Char(10);                                                                            
        Orespuesta Char(1);                                                                         
       End-Pr;                                                                                      
       Dcl-Pr PgmConversion Extpgm('VOTREP00/VTMCSBCR');                                            
        Icia Char(3);                                                                               
        IcedLar Char(30);                                                                           
        IcedCor Char(9);                                                                            
       End-Pr;                                                                                      
       Dcl-Pr ValidarEmail Extpgm('VOTREP00/VTMGOBTR');                                             
        PrtStatus Char(8);                                                                          
        Pemail Char(200);                                                                           
       End-Pr;                                                                                      
       Dcl-Pr EnviaCorreo Extpgm('VOTREP00/VTEPWB3R');                                              
        Pcia Char(3);                                                                               
        Pres Char(2);                                                                               
        Premitente Char(200);                                                                       
        Pdestinatario Char(200);                                                                    
        PRemMostrar Char(1000);                                                                     
        PAsunto Char(1000);                                                                         
        Pmensaje Char(32767);                                                                       
        PReplyto Char(10000);                                                                       
        Pcc Char(10000);                                                                            
        Pcco Char(10000);                                                                           
        Soapr Char(32767);                                                                          
       End-Pr;                                                                                      
       Dcl-Pr PgmCampa Extpgm('VTMCSBKR');                                                          
        Pcia Char(3);                                                                               
        PCamE Char(4);                                                                              
        PCamR Char(4);                                                                              
        POper Char(1);                                                                              
        Pnume Char(3);                                                                              
       End-Pr;                                                                                      
                                                                                                    
       Dcl-Pr PgmNombreComerciales Extpgm('VOTREP00/VTPLDITR');                                     
        Pcia Char(3);                                                                               
        Hrol Char(2);                                                                               
        Hfec Char(10);                                                                              
        Htexto Char(200);                                                                           
       End-Pr;                                                                                      
                                                                                                    
       //------------------------------------------------------------------/                        
       // Inicio Programa Principal                                        /                        
       //------------------------------------------------------------------/                        
        PrBorrarFile();                                                                             
        PrCrearFile();                                                                              
        String = *Blanks;                                                                           
        String = 'Select Char(CccmpÑ) ' +                                                           
            'From Opcapf/Lcacmp01 ' +                                                               
            'Where Ccdele = '' ''' + ' And ' +                                                      
            'Substr(Ccname, 30, 1) <> ''P''';                                                       
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur0 From :String;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor0 Cursor For Cur0;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor0;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cursor0 Into :WPais;                                                                 
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         EvalR WPais = %Trim(WPais);                                                                
         WPais = %Xlate(' ':'0':WPais);                                                             
         If WPais <> '002';                                                                         
          NroPd = *Zeros;                                                                           
                                                                                                    
          Exec Sql                                                                                  
           Select Pgrnro Into :NroC                                                                 
           From Votrea00/Vtmpgrf                                                                    
           Where Pgrest = ' ' And Pgrcia = :Wpais;                                                  
           Nroc += 1;                                                                               
                                                                                                    
          FecUefe = *Blanks;                                                                        
          Exec Sql                                                                                  
           Select Cgtnro, Cgtfue Into :NroPd, :FecUefe                                              
           From Votrea00/Vtmcgtf                                                                    
           Where Cgtcia = :Wpais And Cgtest = ' ';                                                  
                                                                                                    
          If %Len(%Trim(FecUefe)) > *Zeros;                                                         
           PrGuiasActivasPorZona();                                                                 
           Exec Sql                                                                                 
            Update Votrea00/Vtmcgtf Set Cgtfue = Char(Current Date, Iso)                            
            Where Cgtcia = :Wpais And Cgtest = ' ';                                                 
          Endif;                                                                                    
         Endif;                                                                                     
         Exec Sql                                                                                   
          Fetch Cursor0 Into :WPais;                                                                
        Enddo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor0;                                                                             
                                                                                                    
       *Inlr = *On;                                                                                 
       Return;                                                                                      
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Archivo Creado en la Qtemp               //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrBorrarFile;                                                                       
        Exec Sql                                                                                    
         Drop Table Qtemp/Guias;                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //----------------------------------------------------------------//                         
       // Procedimiento que Crear Archivo en la Qtemp                    //                         
       //----------------------------------------------------------------//                         
       Dcl-Proc PrCrearFile;                                                                        
        Exec Sql                                                                                    
         Create Table Qtemp/Guias(                                                                  
          Cia Char(3) Ccsid 284 Default Null,                                                       
          Zna Char(3) Ccsid 284 Default Null,                                                       
          Cmp Char(4) Ccsid 284 Default Null,                                                       
          Ced Char(30) Ccsid 284 Default Null);                                                     
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta las Guías Activas por País y Zona     //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrGuiasActivasPorZona;                                                              
        Dcl-s Icaso Char(7) Inz(*Blanks);                                                           
        Dcl-s Icedula Char(30) Inz(*Blanks);                                                        
        Dcl-s Ifecha Char(10) Inz(*Blanks);                                                         
        Dcl-s Orespuesta Char(1) Inz(*Blanks);                                                      
                                                                                                    
        Exec Sql                                                                                    
         Declare Cur1Z Cursor For                                                                   
         Select Gldzon                                                                              
         From Votrea00/Vtmgldf                                                                      
         Where Gldest = ' ' And Gldcia = :Wpais                                                     
         Group By Gldzon;                                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open Cur1Z;                                                                                
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cur1Z Into :Wzona;                                                                   
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         PrGuiasActivas();                                                                          
         Exec Sql                                                                                   
          Fetch Cur1Z Into :Wzona;                                                                  
        Enddo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cur1Z;                                                                               
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta las Guías Activas por País            //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrGuiasActivas;                                                                     
        Dcl-s Icaso Char(7) Inz(*Blanks);                                                           
        Dcl-s Icedula Char(30) Inz(*Blanks);                                                        
        Dcl-s Ifecha Char(10) Inz(*Blanks);                                                         
        Dcl-s Orespuesta Char(1) Inz(*Blanks);                                                      
                                                                                                    
        Exec Sql                                                                                    
         Declare Cur1 Cursor For                                                                    
         Select Gldcte, Gldcin, Gldzon, Gldfin                                                      
         From Votrea00/Vtmgldf                                                                      
         Where Gldest = ' ' And Gldcia = :Wpais                                                     
               And Gldzon = :Wzona                                                                  
         Order By Gldcin, Gldzon, Gldcte;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open Cur1;                                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cur1 Into :Cedula, :CmpIng, :Wzona, :WfechaI;                                        
                                                                                                    
        NroGuias = *Zeros;                                                                          
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         Icedula = %Trim(%Editc(Cedula:'Z'));                                                       
         Icaso = *Blanks;                                                                           
         Ifecha = *Blanks;                                                                          
         Orespuesta = *Blanks;                                                                      
         PgmNewPlanG(Wpais:Wzona:Icaso:Icedula:Ifecha:Orespuesta);                                  
         If Orespuesta  = 'S';                                                                      
          PrVerificarRut();                                                                         
         Endif;                                                                                     
         Exec Sql                                                                                   
          Fetch Cur1 Into :Cedula, :CmpIng, :Wzona, :WfechaI;                                       
        Enddo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cur1;                                                                                
                                                                                                    
        If NroGuias > *Zeros;                                                                       
         PrEnviarEmail();                                                                           
        Endif;                                                                                      
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Verifica si la Guía Tiene el RUT Actualizado   //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrVerificarRut;                                                                     
        Dcl-s NroCp Packed(3:0) Inz(*Zeros);                                                        
        Dcl-s Tfecha Char(10) Inz(*Blanks);                                                         
        Dcl-s Kfecha Char(10) Inz(*Blanks);                                                         
        Dcl-s Dateln Date;                                                                          
        Dcl-s Dataw  Date;                                                                          
                                                                                                    
        Tfecha = FecUefe;                                                                           
        Kfecha = %Char(%Date());                                                                    
        IcedLar = *Blanks;                                                                          
        IcedCor = %Trim(%Editc(Cedula:'Z'));                                                        
        PgmConversion(Wpais:IcedLar:IcedCor);                                                       
        NroCp = *Zeros;                                                                             
        Exec Sql                                                                                    
         Select Count(0) Into :NroCp                                                                
         From Votrea00/Vtmrtgf                                                                      
         Where Rtgcia = :Wpais And (Rtgidt = :Cedula Or                                             
               Trim(Char(Rtgidt)) = Trim(:IcedLar))                                                 
               And Rtgest = ' ';                                                                    
                                                                                                    
        If NroCp <= *Zeros;                                                                         
         Dateln = %Date(Kfecha:*Iso);                                                               
         Dataw = %Date(Tfecha:*Iso);                                                                
         If %Diff(Dateln:Dataw:*Days) >= NroPd;                                                     
          NroGuias += 1;                                                                            
          String = *Blanks;                                                                         
          String = 'Insert Into Qtemp/Guias(Cia, Zna, Cmp, Ced) ' +                                 
                   'Values(' + '''' + Wpais + '''' + ', ' +                                         
                   '''' + %Trim(Wzona) + '''' + ', ' +                                              
                   '''' + %Trim(CmpIng) + '''' + ', ' +                                             
                   '''' + %Trim(IcedLar) + '''' + ')';                                              
                                                                                                    
          Exec Sql                                                                                  
           Execute Immediate :String;                                                               
                                                                                                    
         Endif;                                                                                     
        Endif;                                                                                      
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Envia Correo a Personas del Area Comercial     //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrEnviarEmail;                                                                      
        Dcl-s Pcia Char(3) Inz(*Blanks);                                                            
        Dcl-s Pres Char(2) Inz(*Blanks);                                                            
        Dcl-s Premitente Char(200) Inz(*Blanks);                                                    
        Dcl-s Pdestinatario Char(200) Inz(*Blanks);                                                 
        Dcl-s PRemMostrar Char(1000) Inz(*Blanks);                                                  
        Dcl-s PAsunto Char(1000) Inz(*Blanks);                                                      
        Dcl-s Pmensaje Char(32767) Inz(*Blanks);                                                    
        Dcl-s PmensajeX Char(32767) Inz(*Blanks);                                                   
        Dcl-s PmensajeW Char(32767) Inz(*Blanks);                                                   
        Dcl-s PReplyto Char(10000);                                                                 
        Dcl-s Pcc Char(10000);                                                                      
        Dcl-s Pcco Char(10000);                                                                     
        Dcl-s Soapr Char(32767);                                                                    
        Dcl-s w_FechaC Char(10) Inz(*Blanks);                                                       
        Dcl-s W_FechaN Char(10) Inz(*Blanks);                                                       
        Dcl-s Idx Packed(5:0) Inz(*Zeros);                                                          
        Dcl-s Kcedula Char(30) Inz(*Zeros);                                                         
        Dcl-s Wcmp Char(4) Inz(*Zeros);                                                             
        Dcl-s Tesw Char(1) Inz('N');                                                                
                                                                                                    
       // Asigna compañia                                                                           
       Pcia= Wpais;                                                                                 
       Hrol = 'GS';                                                                                 
       Htexto = *Blanks;                                                                            
       PgmNombreComerciales(Pcia:Hrol:Hfec:Htexto);                                                 
       Wtexto = *Blanks;                                                                            
       If %Len(%Trim(Htexto)) <= *Zeros;                                                            
        Wtexto = 'Guía';                                                                            
       Else;                                                                                        
        Wtexto = %Trim(HtextoC);                                                                    
       Endif;                                                                                       
       Hrol = 'CC';                                                                                 
       HtextoC = *Blanks;                                                                           
       PgmNombreComerciales(Pcia:Hrol:Hfec:HtextoC);                                                
       WtextoC = *Blanks;                                                                           
       If %Len(%Trim(HtextoC)) <= *Zeros;                                                           
        WtextoC = 'Compradora';                                                                     
       Else;                                                                                        
        WtextoC = %Trim(HtextoC);                                                                   
       Endif;                                                                                       
       // Asigna Respuesta                                                                          
       Pres= *Blanks;                                                                               
       // Asigna Remitente                                                                          
       Premitente = 'servicioalcliente@leonisa.com';                                                
       Wnombre = *Blanks;                                                                           
       If %Trim(IcedLar) <> '0';                                                                    
        PrInformacionCp();                                                                          
       Endif;                                                                                       
       // Destinatarios a Enviar                                                                    
       Pdestinatario = *Blanks;                                                                     
       Wlider = PrLiderDet();                                                                       
       Wregional = PrRegional();                                                                    
       Wcomercial = PrComercial();                                                                  
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
       // Se Activo la Guía Correctamente                                                           
        // Asigna Descripcion mensaje                                                               
        PRemMostrar= %Trim(Wtexto) +                                                                
                     ' sin enviar la información del RUT zona(' +                                   
                     %Trim(Wzona) + ').';                                                           
        // Asigna asunto                                                                            
        Pasunto = %Trim(Wtexto) +                                                                   
                  ' sin enviar la información del RUT zona(' +                                      
                  %Trim(Wzona) + ').';                                                              
                                                                                                    
        // Asigna asunto                                                                            
        Pmensaje   = 'Cordial saludo, <br><br>' +                                                   
                     'En la zona ' + %Trim(Wzona) + ' las siguientes ' +                            
        %Trim(WtextoC) + ' inscritas en el plan se encuentran ' +                                   
                     'pendientes por el documento del RUT: <br><br>';                               
                                                                                                    
        PmensajeW = *Blanks;                                                                        
        TesW = 'N';                                                                                 
        For Idx = 1 To NroGuias;                                                                    
         Exec Sql                                                                                   
          Select Cia, Ced, Cmp Into :Wpais, :Kcedula, :Wcmp                                         
          From Qtemp/Guias                                                                          
          Where Rrn(Guias) = :Idx And Zna = :Wzona;                                                 
                                                                                                    
          If SqlCod <> 100 And SqlCod >= *Zeros;                                                    
           TesW = 'S';                                                                              
          Endif;                                                                                    
                                                                                                    
          PCamE = Wcmp;                                                                             
          PCamR = *Blanks;                                                                          
          POper = '+';                                                                              
                                                                                                    
          Exec Sql                                                                                  
           Select Digits(:NroC) Into :Pnume                                                         
           From Sysibm/Sysdummy1;                                                                   
                                                                                                    
          PgmCampa(Wpais:PCamE:PCamR:POper:Pnume);                                                  
          IcedCor = *Blanks;                                                                        
          IcedLar = *Blanks;                                                                        
          IcedLar = %Trim(Kcedula);                                                                 
          PgmConversion(Wpais:IcedLar:IcedCor);                                                     
          Cedula = %Int(%Trim(IcedCor));                                                            
                                                                                                    
         PrInformacionCp();                                                                         
         PmensajeX = *Blanks;                                                                       
         PmensajeX = 'C.C ' + %Trim(IcedLar) + ' Nombre ' + %Trim(Wnombre) +                        
                     ' ' + 'Campaña inscrita ' + %Trim(Wcmp) + ', en ' +                            
                     'caso de no scanear el documento y que este sea vali' +                        
        'dado de manera correcta, la ' + %Trim(WtextoC) + ' se desactiva' +                         
                     'ra automáticamente del plan en C' + %Trim(PCamR) +                            
                     ' <br><br> ';                                                                  
         PmensajeW = %Trim(PmensajeW) + ' ' + %Trim(PmensajeX);                                     
        EndFor;                                                                                     
       Pmensaje = %Trim(Pmensaje) + ' ' + %Trim(PmensajeW);                                         
       PReplyto   = *Blanks;                                                                        
       Pcc        = *Blanks;                                                                        
       Pcco       = *Blanks;                                                                        
       Soapr      = *Blanks;                                                                        
                                                                                                    
       If Tesw = 'S';                                                                               
        TesW = 'N';                                                                                 
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
                    Soapr           );                                                              
       Endif;                                                                                       
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta la Información de la Compradora       //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrInformacionCp;                                                                    
        Dcl-s SqlStr Char(1000) Inz(*Blanks);                                                       
                                                                                                    
        Wnombre = *Blanks;                                                                          
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Select Trim(Mlname) ' +                                                           
                 'From Opcapf/Pmllst' + Wpais + ' ' +                                               
                 'Where MlcstÑ = ' + %Trim(%Editc(Cedula:'Z')) + ' ' +                              
                 'Fetch First 1 Row Only';                                                          
                                                                                                    
        Exec Sql                                                                                    
         Prepare CurA From :SqlStr;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare CursorA Cursor For CurA;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open CursorA;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch CursorA Into :Wnombre;                                                               
                                                                                                    
        If Sqlcod = 100 Or Sqlcod < *Zeros;                                                         
         Wnombre = *Blanks;                                                                         
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close CursorA;                                                                             
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email de la Líder                  //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrLiderDet;                                                                         
        Dcl-Pi *N Char(1000) End-Pi;                                                                
        Dcl-s Email Char(200) Inz(*Blanks);                                                         
        Dcl-s Wlider Char(1000) Inz(*Blanks);                                                       
        Dcl-s SqlStr Char(1000) Inz(*Blanks);                                                       
                                                                                                    
        Region = *Blanks;                                                                           
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Select Daleml, Sminf2 ' +                                                         
                 'From Vpcopa00/Vtmdalf Inner Join Opf' + Wpais +                                   
                 '/Pslman On Trim(Sminf1) = Trim(Dalced) ' +                                        
                'Where Trim(Smslsp) = ' + '''' + Wzona + '''' + ' And ' +                           
                'Smdele = '' ''' + ' ' +                                                            
                'Fetch First 1 Row Only';                                                           
                                                                                                    
        Exec Sql                                                                                    
         Prepare CurB From :SqlStr;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare CursorB Cursor For CurB;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open CursorB;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch CursorB Into :Email, :Region;                                                        
                                                                                                    
        If Sqlcod = 100 Or Sqlcod < *Zeros;                                                         
         Email = *Blanks;                                                                           
         Region = *Blanks;                                                                          
        Endif;                                                                                      
                                                                                                    
       Respuesta = *Blanks;                                                                         
       w_Email = *Blanks;                                                                           
       w_Email = Email;                                                                             
       ValidarEmail(Respuesta:w_Email);                                                             
       If %Trim(Respuesta) = 'Invalido';                                                            
        Wlider = *Blanks;                                                                           
       Else;                                                                                        
        Wlider = Email;                                                                             
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CursorB;                                                                              
                                                                                                    
       Return Wlider;                                                                               
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email del Regional                 //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrRegional;                                                                         
        Dcl-Pi *N Char(1000) End-Pi;                                                                
        Dcl-s Email Char(200) Inz(*Blanks);                                                         
        Dcl-s Wregional Char(1000) Inz(*Blanks);                                                    
        Dcl-s SqlStr Char(1000) Inz(*Blanks);                                                       
                                                                                                    
        SqlStr = *Blanks;                                                                           
        SqlStr = 'Select Divemd ' +                                                                 
                 'From Votrea00/Vtmdivf ' +                                                         
                 'Where Trim(Divpai) = ' + '''' + Wpais + '''' + ' And '+                           
                'Divest = '' ''' + ' And Divdiv =' + '''' + %Trim(Region)                           
                 + '''' + ' ' +                                                                     
                 'Fetch First 1 Row Only';                                                          
                                                                                                    
        Exec Sql                                                                                    
         Prepare CurC From :SqlStr;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare CursorC Cursor For CurC;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open CursorC;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch CursorC Into :Email;                                                                 
                                                                                                    
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
         Close CursorC;                                                                             
                                                                                                    
        Return Wregional;                                                                           
                                                                                                    
       End-Proc;                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta el Email del Area Comercial           //                       
       //------------------------------------------------------------------//                       
       Dcl-Proc PrComercial;                                                                        
        Dcl-Pi *N Char(1000) End-Pi;                                                                
        Dcl-s Email Char(200) Inz(*Blanks);                                                         
        Dcl-s Wcomercial Char(1000) Inz(*Blanks);                                                   
        Dcl-s SqlStr Char(1000) Inz(*Blanks);                                                       
        Dcl-s Nro Packed(3:0) Inz(*Zeros);                                                          
        Dcl-s Coma Char(1) Inz(',');                                                                
                                                                                                    
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Seceml ' +                                                                  
                'From Votrea00/Vtmsecf ' +                                                          
                'Where Trim(Seccia) = ' + '''' + Wpais + '''' + ' And ' +                           
                'Trim(Secdiv) = ' + '''' + %Trim(Region) + '''' + ' And ' +                         
                'Secest = '' ''';                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Prepare CurD From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare CursorD Cursor For CurD;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open CursorD;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch CursorD Into :Email;                                                                  
                                                                                                    
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
         WComercial = %Trim(WComercial) + %Trim(Email) + %Trim(Coma);                               
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch CursorD Into :Email;                                                                 
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CursorD;                                                                              
                                                                                                    
       Return WComercial;                                                                           
                                                                                                    
       End-Proc;                                                                                    
