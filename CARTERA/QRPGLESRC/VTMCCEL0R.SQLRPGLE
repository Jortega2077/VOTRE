      *********************************************************************                         
      *                                                                   *                         
      * Programa      : VTSPPPTGR                                         *                         
      * Descripcion   : Trigger del Vteuctf                               *                         
      * Proyecto      :                                                   *                         
      * Autor         : Leonel Mauricio Parra Suásez - PersonalSoft       *                         
      * Responsable   : JOrtega                                           *                         
      * Fecha Creacion: 11 de Diciembre 2018                              *                         
      *                                                                   *                         
      *********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
      * Definicion de Variables de Trabajo                                                          
     DParm2            s              7b 0                                                          
     DWuser            s             20a   Inz(*Blanks)                                             
     DWhrmd            s              8a   Inz(*Blanks)                                             
     DWfcmd            s             10a   Inz(*Blanks)                                             
     DWsw              s              1a   Inz(*Blanks)                                             
     DWcpmd            s              4a   Inz(*Blanks)                                             
     D$Prms            s              1                                                             
     D$Optn            s             10                                                             
     D$Lbrs            s             10                                                             
     D$Refe            s             10                                                             
     D$Mbre            s             10                                                             
     DParm             s              1    Inz(*Blanks)                                             
     D Ts              s               z                                                            
     DVariable         s             10    Inz(*Blanks)                                             
     D Cmd             s            500A   Inz(*Blanks)                                             
     DCmdData          s            500a   Inz(*Blanks)                                             
                                                                                                    
      * Definicion de Estructuras de Trabajo                                                        
     DParm1            Ds         32767                                                             
     D  Pfname                       10                                                             
     D  Pflibrary                    10                                                             
     D  Pfmember                     10                                                             
     D  Trgevent                      1                                                             
     D  Trgtime                       1                                                             
     D  Trgcmtlvl                     1                                                             
     D  Filler1                       3                                                             
     D  Trgccsid                      7b 0                                                          
     D  Filler2                       8                                                             
     D  Oroffset                      7b 0                                                          
     D  Orrcdlen                      7b 0                                                          
     D  Ornbmapofs                    7b 0                                                          
     D  Ornbmaplen                    7b 0                                                          
     D  Nwoffset                      7b 0                                                          
     D  Nwrcdlen                      7b 0                                                          
     D  Nwnbmapofs                    7b 0                                                          
     D  Nwnbmaplen                    7b 0                                                          
     D  Dataspace              1  32767                                                             
     D  Bytes                         1    Overlay(Dataspace) Dim(32767)                            
                                                                                                    
     DFecha            Ds                                                                           
     DAno                      1      4                                                             
     DMes                      5      6                                                             
     DDia                      7      8                                                             
     DFinfecha                 1      8                                                             
                                                                                                    
     DHora             Ds                                                                           
     DHoras                    1      2                                                             
     DMinutos                  3      4                                                             
     DSegun                    5      6                                                             
     DFinhora                  1      6  0                                                          
                                                                                                    
      * Area de Datos del Sistema                                                                   
     DAs400           Sds                                                                           
     DUsrsys                 254    263                                                             
     DSig_sys                195    196  0                                                          
     DMes_sys                276    277  0                                                          
     DDia_sys                278    279  0                                                          
     DAÑo_sys                280    281  0                                                          
                                                                                                    
      * Nombramiento del Registro Original(Viejo)                                                   
     D@Regviejo        s               *   Inz(*Null)                                               
     DRegviejo       e Ds                  Extname(Vteuctf) Prefix(V)                               
     D                                     Based(@Regviejo)                                         
                                                                                                    
      * Nombramiento del Registro Nuevo Vtmsacf                                                     
     D@Regnuevo        s               *   Inz(*Null)                                               
     DRegnuevo       e Ds                  Extname(Vteuctf) Prefix(N)                               
     D                                     Based(@Regnuevo)                                         
                                                                                                    
      * Definicion de Variables Constantes                                                          
     DCl_None          c                   Const('0')                                               
     DCl_Chg           c                   Const('1')                                               
     DCl_Cs            c                   Const('2')                                               
     DCl_All           c                   Const('3')                                               
                                                                                                    
      * Null Byte Dtatus                                                                            
     DNb_Notnull       c                   Const('0')                                               
     DNb_Null          c                   Const('1')                                               
                                                                                                    
      * Trigger Event                                                                               
     DTe_Insert        c                   Const('1')                                               
     DTe_Delete        c                   Const('2')                                               
     DTe_Update        c                   Const('3')                                               
                                                                                                    
      * Trigger Time                                                                                
     DTt_After         c                   Const('1')                                               
     DTt_Before        c                   Const('2')                                               
                                                                                                    
     D ArchivoTrigger  Pr                                                                           
     D Auditoria       Pr                                                                           
     DParm                            1                                                             
                                                                                                    
     DQuienLoInvocoPgm...                                                                           
     D                 Pr                  Extpgm('VOTREP00/VTMGOBQR')                              
     D pTipoOperacion                 1                                                             
     D pProgramaLlamador...                                                                         
     D                               10                                                             
     ** Definición de Prototipos de Trabajo                                                         
      // Definicion Prototipo para un Comando Externo QCMDEXC.                                      
     D RunSysCmd       Pr                  ExtPgm('QCMDEXC')                                        
     D Cmd                          500a   Options(*VarSize) Const                                  
     D CmdLen                        15p 5 Const                                                    
                                                                                                    
       //----------------------------------------------------------------//                         
       //            P r o g r a m a    P r i n c i p a l                //                         
       //----------------------------------------------------------------//                         
      /Free                                                                                         
        // Le Asigna al Registro el Espacio de Datos al Pmllst001                                   
        @Regviejo = %Addr(Bytes(Oroffset + 1));                                                     
        @Regnuevo = %Addr(Bytes(Nwoffset + 1));                                                     
                                                                                                    
        Clear CmdData;                                                                              
        CmdData = 'OVRDBF FILE(VTWUCTF) TOFILE(VOTREA00/VTWUCTF) '                                  
                  + 'OVRSCOPE(*JOB)';                                                               
        CallP(E) RunSysCmd(CmdData: %Size(CmdData));                                                
        ArchivoTrigger();                                                                           
       Clear CmdData;                                                                               
       CmdData = 'DLTOVR FILE(VTWUCTF) LVL(*JOB)';                                                  
       CallP(E) RunSysCmd(CmdData: %Size(CmdData));                                                 
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      *-------------------------------------------------------------------*                         
      * Rutina Inicial del Programa Parametros de Entrada                 *                         
      *-------------------------------------------------------------------*                         
     C     *Inzsr        Begsr                                                                      
      * Definicion de Parametros de Entrada                                                         
     C     *Entry        Plist                                                                      
     C                   Parm                    Parm1                                              
     C                   Parm                    Parm2                                              
     C                   Endsr                                                                      
                                                                                                    
     P*-------------------------------------------------------------------*                         
     P* Nombre del Procedimiento: ArchivoTrigger                          *                         
     P*-------------------------------------------------------------------*                         
     P ArchivoTrigger  B                                                                            
     D ArchivoTrigger  Pi                                                                           
      /Free                                                                                         
       // Evento 1 : Ingreso                                                                        
       If Trgevent = Te_Insert;                                                                     
          Parm = 'I';                                                                               
          QuienLoInvocoPgm(Trgevent:Variable);                                                      
          Auditoria(Parm);                                                                          
       // Evento 3 : Modificacion                                                                   
        ElseIf Trgevent = Te_Update;                                                                
         Parm = 'M';                                                                                
         QuienLoInvocoPgm(Trgevent:Variable);                                                       
         Auditoria(Parm);                                                                           
        Endif;                                                                                      
      /End-Free                                                                                     
     P ArchivoTrigger  E                                                                            
                                                                                                    
     P*------------------------------------------------------------------*                          
     P* Nombre del Procedimiento: Auditoria                              *                          
     P* Deja Registro de la Parsona que Toco Alguno de los Archivos      *                          
     P*------------------------------------------------------------------*                          
     P Auditoria       B                                                                            
     D Auditoria       Pi                                                                           
     DParm                            1                                                             
      /Free                                                                                         
       Select;                                                                                      
       When Parm = 'I';                                                                             
        If NUCTCTO <> 0 And NUCTCTE <> 0;                                                           
            Clear NUctest;                                                                          
            Clear NUctesq;                                                                          
            Clear NUctsbq;                                                                          
            Exec Sql                                                                                
            Insert Into Votrea00/Vtwuctf (Tuccod,Tuccto,Tucvst,Tucvsq,                              
                                 Tucvbq,Tucnst,Tucnsq,Tucnbq,                                       
                                 Tucpgm,Tucfec,Tuchor,Tucusr)                                       
                       Values(:NUCTCTO, :NUCTCTE,:NUCTEST , :NUCTESQ ,                              
                               :NUCTSBQ, :NUCTEST , :NUCTESQ , :NUCTSBQ ,                           
                              :Variable , Current date , Current Time , User );                     
        Endif;                                                                                      
       When Parm = 'M';                                                                             
        If VUCTCTO <> 0 And VUCTCTE <> 0 And                                                        
           NUCTCTO <> 0 And NUCTCTE <> 0;                                                           
            Exec Sql                                                                                
            Insert Into Votrea00/Vtwuctf (Tuccod,Tuccto,Tucvst,Tucvsq,                              
                                 Tucvbq,Tucnst,Tucnsq,Tucnbq,                                       
                                 Tucpgm,Tucfec,Tuchor,Tucusr)                                       
                        Values(:VUCTCTO, :VUCTCTE,:VUCTEST , :VUCTESQ ,                             
                               :VUCTSBQ, :NUCTEST , :NUCTESQ , :NUCTSBQ ,                           
                              :Variable , Current date , Current Time , User );                     
        Endif;                                                                                      
       EndSl;                                                                                       
      /End-Free                                                                                     
     P Auditoria       E                                                                            
