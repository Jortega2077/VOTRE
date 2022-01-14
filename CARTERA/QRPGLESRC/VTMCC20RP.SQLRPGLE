     **                                                                   *                         
     ** Programa      : VTMCC20R                                          *                         
     ** Descripciòn:  : Automatización de Cargar Archivos Bancos          *                         
     ** Proyecto      : Carga Bancos                                      *                         
     ** Autor         : Leonel Mauricio Parra Suárez - PersonalSoft S.A.S *                         
     ** Responsable   : JOrtega                      - Leonisa S.A.S      *                         
     ** Fecha Creaciòn: 06 de Agosto de 2018                              *                         
     **                                                                   **                        
     ******************************************************************** **                        
     ** Crear Modulo:                                                     **                        
     **                                                                   **                        
     ** ===> CRTSQLRPGI OBJ(VOTREP00/VTMCC20R) SRCFILE(VOTREP00/QRPGLESRC)**                        
     **      OBJTYPE(*MODULE) TEXT('Automatización de Cargar Archivos     **                        
     **                             Bancos')                              **                        
     **  Después de compilar el módulo, debe crearse el programa con el   **                        
     **  Siguiente comando:                                               **                        
     **                                                                   **                        
     **  CRTPGM     PGM(VOTREP00/VTMCC20RP)                               **                        
     **               MODULE(VOTREP00/VTMCC20RP                           **                        
     **               VOTREP00/VTMCC21RP VOTREP00/VTMCC22RP               **                        
     **               VOTREP00/VTMCC23RP VOTREP00/VTMCC24RP)              **                        
     **                                  ACTGRP(SUBIDABANC)               **                        
     **                                                                   **                        
     ******************************************************************** **                        
     ** Definición de Directivas de Compilación                                                     
     H Bnddir('HTTPAPI':'QC2LE')                                                                    
     H Timfmt(*Hms)                                                                                 
     H Option(*NoDebugIO: *ShowCpy:*NoXRef:*NoExpDds:*NoSecLvl:*NoExt:*SrcStMt)                     
     H DatFmt(*Iso) Aut(*all)                                                                       
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     D Compania                       3                                                             
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     D Compania                       3                                                             
                                                                                                    
     ***************************************************                                            
     D $c              c                   ''''                                                     
     DPosix            s              2s 0 Inz(*Zeros)                                              
     DAplicacion       s              3a   Inz('BAN')                                               
     DFile             s             20a   Inz('Blanks')                                            
     DRuta             s             64a   Inz('Blanks')                                            
     DNrr2             s              4s 0 Inz(*Zeros)                                              
     DPdcbn1           s              2s 0 Inz(*Zeros)                                              
     DXreg             s           1000    Inz(*Zeros)                                              
     Dmseconds         s              6s 0 inz                                                      
     Dmsecondsa        s              6a   Inz(*Blanks)                                             
     DPapl             s              2                                                             
     DPeve             s              3s 0                                                          
     DParcr            s             50                                                             
     DParch            s             50                                                             
     DPexte            s              4                                                             
     DPlibr            s             10                                                             
     DPparm            s              1                                                             
     DJobs             s           1000    Inz(*Blanks)                                             
                                                                                                    
     D                 DS                                                                           
     DSiglo                    1      2S 0                                                          
     DAÑo                      3      4S 0                                                          
     DMes                      5      6S 0                                                          
     DDia                      7      8S 0                                                          
     DFecha                    1      8S 0                                                          
                                                                                                    
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DFsw              s              1a   Inz(*Blanks)                                             
                                                                                                    
     DFecha2          sds                                                                           
     DSig_sys                195    196  0                                                          
     DAÑo_sys                280    281  0                                                          
     DMes_sys                276    277  0                                                          
     DDia_sys                278    279  0                                                          
     DUser_Name              254    263                                                             
                                                                                                    
     DFields         e Ds                  Extname(Vtmasbf:Regasb)                                  
     D                                     Prefix(Q)                                                
     DFields1        e Ds                  Extname(Vtmftpf:REGFTP)                                  
     D                                     Prefix(Q)                                                
                                                                                                    
     DCmd              s             20                                                             
     DLong             s             15p 5                                                          
     DComando          Pr                  Extpgm('QCMDEXC')                                        
     D                               20    Options(*Varsize)                                        
     D                                     Const                                                    
     DL                              15p 5                                                          
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     D*Subir            pr                  Extpgm('VOTREP00/VTMGOB0L')                             
     DSubir            pr                  Extpgm('VOTREP00/VTMGOBCR')                              
     DCia                             3                                                             
     DPapl                            2                                                             
     DPeve                            3s 0                                                          
     DParcr                          50                                                             
     DParch                          50                                                             
     DPexte                           4                                                             
     DPlibr                          10                                                             
     DPparm                           1                                                             
     DPgmRutaF         Pr                  Extpgm('VOTREP00/VTMCC22R')                              
     DXcia                            3                                                             
     DXRuta                          64                                                             
     DXfile                          20                                                             
     DXreg                         1000                                                             
                                                                                                    
     DFilePar          Pr                                                                           
     DGrabarReg        Pr                                                                           
                                                                                                    
     DXruta            s             64                                                             
     DXfile            s             20                                                             
     DXcdbn            s              2                                                             
     DXcdes            s              3                                                             
     DXtpar            s              1                                                             
     DXtres            s              1                                                             
     DSubirFile        Pr                  Extpgm('VOTREP00/VTMCC23RP')                             
     DXcia                            3                                                             
     DXruta                          64                                                             
     DXfile                          20                                                             
     DXcdbn                           2                                                             
     DXcdes                           3                                                             
     DXtpar                           1                                                             
     DXtres                           1                                                             
     DSubirFTP         Pr                  Extpgm('VOTREP00/VTMCC21RP')                             
     DXcia                            3                                                             
                                                                                                    
     DRfile            s             20a   inz(*Blanks)                                             
     D*PgmRuta          Pr                  Extpgm('VOTREP00/VTMCCC6L')                             
     DPgmRuta          Pr                  Extpgm('VOTREP00/VTMCC21RP')                             
     DRcia                            3                                                             
     DRRuta                          64                                                             
     DRfile                          20                                                             
     D                                                                                              
       Dcl-Pr Fn_SubirArchivoaIFS  Char(3);                                                         
        Rcia           Char( 3);                                                                    
        RRuta          Char(64);                                                                    
        Rfile          Char(20);                                                                    
        dsfileP Likeds(Fields);                                                                     
       End-Pr;                                                                                      
                                                                                                    
     D RunSysCmd       Pr                  ExtPgm('QCMDEXC')                                        
     D Cmd                          500A   Options(*VarSize) Const                                  
     D CmdLen                        15P 5 Const                                                    
     DCmdData          s           1000a   Inz(*Blanks)                                             
                                                                                                    
     DMoverArchivoIfs...                                                                            
     D                 Pr                                                                           
      /Free                                                                                         
        ////////////////////////////////////////////////////////////////////////                    
        //                         PROGRAMA PRINCIPAL                         //                    
        ////////////////////////////////////////////////////////////////////////                    
       Cmd = %Char(%date():*iso);                                                                   
       Clear Fields;                                                                                
       Clear Fields1;                                                                               
       Exec SQl                                                                                     
        Declare C1 Cursor For                                                                       
        Select *                                                                                    
        From VTmAsbf                                                                                
        Inner join Vtmftpf on Trim(FTPAPL)= Trim(Asbapl) and FTPEVT = Asbevt                        
        Where ASBCIA = :Compania                                                                    
          And ASBEST = ' ' And Ftpusr = 'GNACH'                                                     
        Fetch First 1 Row Only;                                                                     
                                                                                                    
       Exec Sql Open C1;                                                                            
       Exec Sql Fetch C1 Into :Fields ,:Fields1;                                                    
                                                                                                    
       Dow Sqlcod = *Zeros;                                                                         
           Pr_ActualizaParametro();                                                                 
           Clear CmdData;                                                                           
           CmdData = 'FTP        RMTSYS('+$c+ QFTPSVR +$c+')';                                      
        // CallP(E) RunSysCmd(CmdData: %Size(CmdData));                                             
           callp Fn_SubirArchivoaIFS(QAsbcia:QFTPRTA:File:Fields);                                  
                                                                                                    
        Clear Jobs;                                                                                 
        Exec Sql                                                                                    
         Select Qsys2.Job_Name Into :Jobs                                                           
         From Sysibm.Sysdummy1;                                                                     
                                                                                                    
        // Borra Spool del FTP                                                                      
         Clear Cmd;                                                                                 
         Cmd = 'DLTSPLF FILE(QSYSPRT) JOB(' + %Trim(Jobs) + ') ' +                                  
               'SPLNBR(*LAST)';                                                                     
         Callp(E) RunSysCmd (Cmd:%Size(Cmd));                                                       
                                                                                                    
           Exec Sql Fetch C1 Into :Fields ,:Fields1;                                                
       EndDo;                                                                                       
       Exec Sql Close C1;                                                                           
       *Inlr = *On;                                                                                 
      /END-FREE                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba Registro en Archivo de Parametros        //                       
       //------------------------------------------------------------------//                       
     PFilePar          B                                                                            
     DFilePar          Pi                                                                           
      /Free                                                                                         
      *Pbccia = Compania;                                                                           
      *Xcdbn = %Xlate(' ':'0':Xcdbn);                                                               
      *Xcdes = %Xlate(' ':'0':Xcdes);                                                               
      *Pbccbn = Xcdbn;                                                                              
      *Pbcces = Xcdes;                                                                              
      *Pbcdes = %Trim(ASBCDB);                                                                      
      *Pbctip = Xtpar;                                                                              
      *Chain (Pbccbn:Pbccia:Pbcces) Vtwpbcf;                                                        
      *If Not %Found(Vtwpbcf);                                                                      
      * Write Regpbc;                                                                               
      *Endif;                                                                                       
      /End-Free                                                                                     
     PFilePar          E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba Registro en Archivo de Parametros        //                       
       //------------------------------------------------------------------//                       
     PPr_ActualizaParametro...                                                                      
     P                 B                                                                            
     DPr_ActualizaParametro...                                                                      
     D                 Pi                                                                           
      /Free                                                                                         
       Exec Sql                                                                                     
        Update input a set FTPDES = Trim(:QFTPUSR)||' '||Trim(:QFTPPWD)                             
        where  rrn(a)=1;                                                                            
       Exec Sql                                                                                     
        Update input a set  FTPDES = 'cd '||:QFTPRTA                                                
        where  rrn(a)=5;                                                                            
      /End-Free                                                                                     
     PPr_ActualizaParametro...                                                                      
     P                 E                                                                            
