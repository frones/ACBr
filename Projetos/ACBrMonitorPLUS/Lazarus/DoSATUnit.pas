unit DoSATUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CmdUnit, pcnConversao, strutils;

procedure DoSAT(Cmd: TACBrCmd);
procedure CarregarDadosVenda(aStr: String; aNomePDF : String = '');
function ParamAsXML(AParam: String): String;
procedure CarregarDadosCancelamento(aStr: String);
function MontaDadosStatusSAT : AnsiString;
function RespostaEnviarDadosVenda( Resultado: String): AnsiString;
procedure GerarIniCFe( AStr: WideString; ApenasTagsAplicacao: Boolean = True);

implementation

uses
  ACBrMonitor1,ACBrUtil,DoACBrUnit,IniFiles, pcnAuxiliar, typinfo,
  ACBrSATExtratoClass;

procedure DoSAT(Cmd: TACBrCmd);
var
  ArqCFe : String;
  Resultado: String;
begin
  with FrmACBrMonitor do
  begin
    try
      if Cmd.Metodo = 'ativar' then
      begin
        if (EstaVazio(Trim(Cmd.Params(0))) and
            EstaVazio(Trim(Cmd.Params(1)))) then
          Cmd.Resposta := ACBrSAT1.AtivarSAT(1, OnlyNumber(edtEmitCNPJ.Text), StrToInt(edtCodUF.Text))
        else
        begin
          if (ACBrSAT1.Config.ide_tpAmb <> taHomologacao) and
             (not ValidarCNPJ(Cmd.Params(0))) then
            raise Exception.Create('CNPJ '+Cmd.Params(0)+' inválido.') ;

          Cmd.Resposta := ACBrSAT1.AtivarSAT(1,Cmd.Params(0), StrToInt(Cmd.Params(1)));
        end
      end

      else if Cmd.Metodo = 'inicializar' then
      begin
        if ACBrSAT1.Inicializado then
          Cmd.Resposta := 'SAT ja inicializado'
        else
        begin
          ACBrSAT1.Inicializar;
          Cmd.Resposta := 'SAT inicializado';
        end;
      end

      else if Cmd.Metodo = 'desinicializar' then
      begin
        if not ACBrSAT1.Inicializado then
          Cmd.Resposta := 'SAT não inicializado'
        else
        begin
          ACBrSAT1.DesInicializar;
          Cmd.Resposta := 'SAT desinicializado'
        end;
      end

      else if Cmd.Metodo = 'associarassinatura' then
      begin
        if (EstaVazio(Trim(Cmd.Params(0))) and
            EstaVazio(Trim(Cmd.Params(1)))) then
          Cmd.Resposta := ACBrSAT1.AssociarAssinatura(edtEmitCNPJ.Text, edtSwHAssinatura.Text)
        else
        begin
          if (ACBrSAT1.Config.ide_tpAmb <> taHomologacao) and
             (not ValidarCNPJ(Cmd.Params(0))) then
            raise Exception.Create('CNPJ '+Cmd.Params(0)+' inválido.') ;

          Cmd.Resposta := ACBrSAT1.AssociarAssinatura(Cmd.Params(0), Cmd.Params(1))
        end
      end

      else if Cmd.Metodo = 'bloquear' then
        Cmd.Resposta := ACBrSAT1.BloquearSAT

      else if Cmd.Metodo = 'desbloquear' then
        Cmd.Resposta := ACBrSAT1.DesbloquearSAT

      else if Cmd.Metodo = 'trocarcodigoativacao' then
        Cmd.Resposta := ACBrSAT1.TrocarCodigoDeAtivacao(Cmd.Params(0),
                               StrToIntDef(cmd.Params(1),1), cmd.Params(2))

      else if Cmd.Metodo = 'consultarsat' then
        Cmd.Resposta := ACBrSAT1.ConsultarSAT

      else if Cmd.Metodo = 'consultarstatusoperacional' then
        Cmd.Resposta := MontaDadosStatusSAT

      else if (Cmd.Metodo = 'consultarsessao') or (Cmd.Metodo = 'consultarnumerosessao')  then
         begin
            ACBrSAT1.CFe.Clear;
            ACBrSAT1.CFeCanc.Clear;

            Cmd.Resposta := ACBrSAT1.ConsultarNumeroSessao(StrToInt(cmd.Params(0)));

            if ACBrSAT1.Resposta.codigoDeRetorno = 6000 then
            begin
               ArqCFe:=ACBrSAT1.CFe.NomeArquivo;
               Cmd.Resposta := '[CFE]'+sLineBreak+
                               'nCFe='+IntToStr(ACBrSAT1.CFe.ide.nCFe)+sLineBreak+
                               IfThen(EstaVazio(ArqCFe),'','Arquivo='+ArqCFe+sLineBreak)+
                               'XML='+ACBrSAT1.CFe.AsXMLString;
            end;

            if ACBrSAT1.Resposta.codigoDeRetorno = 7000 then
            begin
               ArqCFe:=ACBrSAT1.CFeCanc.NomeArquivo;
               Cmd.Resposta := '[CANCELAMENTO]'+sLineBreak+
                               'nCFeCanc='+IntToStr(ACBrSAT1.CFeCanc.ide.nCFe)+sLineBreak+
                               IfThen(EstaVazio(ArqCFe),'','Arquivo='+ArqCFe+sLineBreak)+
                               'XML='+ACBrSAT1.CFeCanc.AsXMLString;
            end;
          end


      else if (Cmd.Metodo = 'atualizasoftware') or (Cmd.Metodo = 'atualizarsoftwaresat') then
        Cmd.Resposta := ACBrSAT1.AtualizarSoftwareSAT

      else if (Cmd.Metodo = 'comunicarcertificado') or
              (Cmd.Metodo = 'comunicarcertificadoicpbrasil') then
        Cmd.Resposta := ACBrSAT1.ComunicarCertificadoICPBRASIL(Cmd.Params(0))

      else if Cmd.Metodo = 'carregardadosvenda' then
        CarregarDadosVenda(Cmd.Params(0))

      else if Cmd.Metodo = 'carregardadoscancelamento' then
        CarregarDadosCancelamento(Cmd.Params(0))

      else if (Cmd.Metodo = 'criarcfe') or
              (Cmd.Metodo = 'criarenviarcfe')  then
      begin
        AjustaACBrSAT;
        GerarIniCFe( Cmd.Params(0), (Cmd.Metodo = 'criarenviarcfe') );

        if (Cmd.Metodo = 'criarcfe') then
        begin
          ArqCFe := '';

          ACBrSAT1.CFe.GerarXML( True ); // Tags da Aplicação
          if cbxSATSalvarCFe.Checked then
          begin
            ArqCFe := ACBrSAT1.CalcCFeNomeArq(ACBrSAT1.ConfigArquivos.PastaEnvio,
                          IntToStrZero(ACBrSAT1.CFe.ide.numeroCaixa,3)+'-'+
                          IntToStrZero(ACBrSAT1.CFe.ide.cNF,6),'-satcfe');
            ACBrSAT1.CFe.SaveToFile(ArqCFe);
          end;

          Cmd.Resposta :=  '[CFE]'+sLineBreak+
                           'nCFe='+IntToStr(ACBrSAT1.CFe.ide.nCFe)+sLineBreak+
                           IfThen(EstaVazio(ArqCFe),'','Arquivo='+ArqCFe+sLineBreak)+
                           'XML='+ACBrSAT1.CFe.AsXMLString;
        end
        else
        begin
          Resultado := ACBrSAT1.EnviarDadosVenda( ACBrSAT1.CFe.AsXMLString );
          Cmd.Resposta := RespostaEnviarDadosVenda( Resultado );
        end;
      end

      else if Cmd.Metodo = 'enviarcfe' then
      begin
        AjustaACBrSAT;

        ArqCFe := ParamAsXML(Cmd.Params(0));
        if ArqCFe = '' then
          Resultado := ACBrSAT1.EnviarDadosVenda
        else
          Resultado := ACBrSAT1.EnviarDadosVenda( ArqCFe );

        Cmd.Resposta := RespostaEnviarDadosVenda( Resultado );
      end

      else if Cmd.Metodo = 'cancelarcfe' then
      begin
        if Cmd.Params(0) <> '' then
          CarregarDadosVenda(Cmd.Params(0));

        Resultado := ACBrSAT1.CancelarUltimaVenda;

        Cmd.Resposta := '[CANCELAMENTO]'+sLineBreak+
                         'Resultado='+Resultado+sLineBreak+
                         'numeroSessao='+IntToStr(ACBrSAT1.Resposta.numeroSessao)+sLineBreak+
                         'codigoDeRetorno='+IntToStr(ACBrSAT1.Resposta.codigoDeRetorno)+sLineBreak+
                         'RetornoStr='+ACBrSAT1.Resposta.RetornoStr+sLineBreak;

        ArqCFe := ACBrSAT1.CFeCanc.NomeArquivo;
        if (ArqCFe <> '') and FileExists(ArqCFe) then
          Cmd.Resposta := Cmd.Resposta + 'Arquivo='+ArqCFe+sLineBreak ;

        Cmd.Resposta := Cmd.Resposta + 'XML='+ACBrSAT1.CFeCanc.AsXMLString;
      end

      else if Cmd.Metodo = 'imprimirextratovenda' then
      begin
        PrepararImpressaoSAT(cmd.Params(1));
        CarregarDadosVenda(cmd.Params(0));
        ACBrSAT1.ImprimirExtrato;
      end

      else if Cmd.Metodo = 'imprimirextratoresumido' then
      begin
        PrepararImpressaoSAT(cmd.Params(1));
        CarregarDadosVenda(cmd.Params(0));
        ACBrSAT1.ImprimirExtratoResumido;
      end

      else if Cmd.Metodo = 'imprimirextratocancelamento' then
      begin
        PrepararImpressaoSAT(cmd.Params(2));
        CarregarDadosVenda(cmd.Params(0));
        CarregarDadosCancelamento(cmd.Params(1));
        ACBrSAT1.ImprimirExtratoCancelamento;
      end

      else if Cmd.Metodo = 'gerarpdfextratovenda' then
      begin
        PrepararImpressaoSAT(cmd.Params(0),true);
        CarregarDadosVenda(cmd.Params(0),cmd.Params(1));
        ACBrSAT1.ImprimirExtrato;

        Cmd.Resposta := '[CFe]'+sLineBreak+
                        'NomeArquivo='+ACBrSAT1.Extrato.NomeArquivo;
      end
      else if Cmd.Metodo = 'extrairlogs' then
        ACBrSAT1.ExtrairLogs(cmd.Params(0))

      else if Cmd.Metodo = 'testefimafim' then
      begin
        AjustaACBrSAT;
        ACBrSAT1.InicializaCFe;
        CarregarDadosVenda(cmd.Params(0));
        Resultado := ACBrSAT1.TesteFimAFim(ACBrSAT1.CFe.GerarXML(True));

        Cmd.Resposta := '[TESTEFIMAFIM]'+sLineBreak+
                        'Resultado='+Resultado+sLineBreak+
                        'numeroSessao='+IntToStr(ACBrSAT1.Resposta.numeroSessao)+sLineBreak+
                        'codigoDeRetorno='+IntToStr(ACBrSAT1.Resposta.codigoDeRetorno)+sLineBreak+
                        'RetornoStr='+ACBrSAT1.Resposta.RetornoStr+sLineBreak+
                        'XML='+ACBrSAT1.CFe.AsXMLString;
      end
      else if Cmd.Metodo = 'setnumerosessao' then
        ACBrSAT1.Tag := StrToIntDef(Trim(cmd.Params(0)), 0)

      else if Cmd.Metodo = 'setlogomarca' then
      begin
        if FileExists(Cmd.Params(0)) then
         begin
           ACBrSATExtratoFortes1.LogoVisible := True;
           ACBrSATExtratoFortes1.PictureLogo.LoadFromFile(Cmd.Params(0));
           edtLogoMarcaNFCeSAT.Text := Cmd.Params(0);
           SalvarIni;
         end
        else
           raise Exception.Create('Arquivo não encontrado.');
      end

      else
        raise Exception.Create(ACBrStr('Comando invalido ('+Cmd.Comando+')'));
    finally
      Cmd.Resposta := Cmd.Resposta + FrmACBrMonitor.RespostaIntegrador;
    end
  end;

end;

procedure CarregarDadosVenda(aStr: String; aNomePDF: String);
begin
  if Trim(aStr) = '' then
    exit;

  with FrmACBrMonitor.ACBrSAT1 do
  begin
    CFe.Clear;
    if (pos(#10,aStr) = 0) and FileExists(aStr) then
      CFe.LoadFromFile(aStr)
    else
      CFe.AsXMLString := ConvertStrRecived(aStr);

    if ( FrmACBrMonitor.ACBrSAT1.Extrato.Filtro = TACBrSATExtratoFiltro(fiPDF) ) then
      Extrato.NomeArquivo := IfThen(aNomePDF <> '', aNomePDF ,
        CalcCFeNomeArq(ConfigArquivos.PastaCFeVenda,CFe.infCFe.ID,'','.pdf'));
  end;

end;

function ParamAsXML(AParam: String): String;
var
  SL : TStringList;
begin
  Result := Trim(AParam);

  if (pos(#10,AParam) = 0) and FileExists(AParam) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile( AParam );
      Result := SL.Text;
    finally
      SL.Free;
    end;
  end;
end;

procedure CarregarDadosCancelamento(aStr: String);
begin
  if Trim(aStr) = '' then
    exit;

  if (pos(#10,aStr) = 0) and FileExists(aStr) then
    FrmACBrMonitor.ACBrSAT1.CFeCanc.LoadFromFile(aStr)
  else
    FrmACBrMonitor.ACBrSAT1.CFeCanc.AsXMLString := ConvertStrRecived(aStr);
end;

function MontaDadosStatusSAT: AnsiString;
begin
  Result := FrmACBrMonitor.ACBrSAT1.ConsultarStatusOperacional;

  if FrmACBrMonitor.ACBrSAT1.Resposta.codigoDeRetorno = 10000 then
  begin
    with FrmACBrMonitor.ACBrSAT1.Status do
    begin
      Result := '[StatusSAT]' + sLineBreak ;
      Result := Result + 'NSERIE = '+NSERIE + sLineBreak;
      Result := Result + 'LAN_MAC = '+LAN_MAC+ sLineBreak;
      Result := Result + 'STATUS_LAN = '+StatusLanToStr(STATUS_LAN)+ sLineBreak;
      Result := Result + 'NIVEL_BATERIA = '+NivelBateriaToStr(NIVEL_BATERIA)+ sLineBreak;
      Result := Result + 'MT_TOTAL = '+MT_TOTAL+ sLineBreak;
      Result := Result + 'MT_USADA = '+MT_USADA+ sLineBreak;
      Result := Result + 'DH_ATUAL = '+DateTimeToStr(DH_ATUAL)+ sLineBreak;
      Result := Result + 'VER_SB = '+VER_SB+ sLineBreak;
      Result := Result + 'VER_LAYOUT = '+VER_LAYOUT+ sLineBreak;
      Result := Result + 'ULTIMO_CFe = '+ULTIMO_CFe+ sLineBreak;
      Result := Result + 'LISTA_INICIAL = '+LISTA_INICIAL+ sLineBreak;
      Result := Result + 'LISTA_FINAL = '+LISTA_FINAL+ sLineBreak;
      Result := Result + 'DH_CFe = '+DateTimeToStr(DH_CFe)+ sLineBreak;
      Result := Result + 'DH_ULTIMA = '+DateTimeToStr(DH_ULTIMA)+ sLineBreak;
      Result := Result + 'CERT_EMISSAO = '+DateToStr(CERT_EMISSAO)+ sLineBreak;
      Result := Result + 'CERT_VENCIMENTO = '+DateToStr(CERT_VENCIMENTO)+ sLineBreak;
      Result := Result + 'ESTADO_OPERACAO = '+EstadoOperacaoToStr(ESTADO_OPERACAO);

    end;
  end;

end;

function RespostaEnviarDadosVenda(Resultado: String): AnsiString;
var
  ArqCFe: String;
begin
  with FrmACBrMonitor do
  begin
    Result := '[ENVIO]'+sLineBreak+
              'Resultado='+Resultado+sLineBreak+
              'numeroSessao='+IntToStr(ACBrSAT1.Resposta.numeroSessao)+sLineBreak+
              'codigoDeRetorno='+IntToStr(ACBrSAT1.Resposta.codigoDeRetorno)+sLineBreak+
              'RetornoStr='+ACBrSAT1.Resposta.RetornoStr+sLineBreak;

    ArqCFe := ACBrSAT1.CFe.NomeArquivo;
    if (ArqCFe <> '') and FileExists(ArqCFe) then
      Result := Result + 'Arquivo='+ArqCFe+sLineBreak;

    Result := Result + 'XML='+ACBrSAT1.CFe.AsXMLString;
  end;
end;

procedure GerarIniCFe(AStr: WideString; ApenasTagsAplicacao: Boolean = True);
var
  INIRec : TMemIniFile ;
  OK     : Boolean;
  I, J   : Integer;
  sSecao, sFim, sCodPro : String;

  function RegTribDescToStr(const AInteger: Integer): String;
  begin
    Result := RegTribToStr(TpcnRegTrib(AInteger));
  end;

  function RegTribISSQNDescToStr(const AInteger: Integer): String;
  begin
    Result := RegTribISSQNToStr(TpcnRegTribISSQN(AInteger));
  end;

  function indRatISSQNDescToStr(const AInteger: Integer): String;
  begin
    Result := indRatISSQNToStr(TpcnindRatISSQN(AInteger));
  end;

begin
  INIRec := LerConverterIni(AStr);
  try
    with FrmACBrMonitor do
     begin
       ACBrSAT1.InicializaCFe;

       with ACBrSAT1.CFe do
        begin

          ACBrSAT1.Config.infCFe_versaoDadosEnt := StringToFloatDef( INIRec.ReadString('infCFe','versao',''),ACBrSAT1.Config.infCFe_versaoDadosEnt) ;

          infCFe.versaoDadosEnt := ACBrSAT1.Config.infCFe_versaoDadosEnt;

          Ide.cUF        := INIRec.ReadInteger( 'Identificacao','cUF' ,UFparaCodigo(INIRec.ReadString(  'Emitente','UF', CodigoParaUF(Ide.cUF))));
          Ide.cNF        := INIRec.ReadInteger( 'Identificacao','Codigo' ,INIRec.ReadInteger( 'Identificacao','cNF' ,Ide.cNF));
          Ide.modelo     := INIRec.ReadInteger( 'Identificacao','Modelo' ,INIRec.ReadInteger( 'Identificacao','mod' ,Ide.modelo));
          Ide.nserieSAT  := INIRec.ReadInteger( 'Identificacao','nserieSAT'  ,Ide.nserieSAT);
          Ide.nCFe       := INIRec.ReadInteger( 'Identificacao','nCFe' ,INIRec.ReadInteger( 'Identificacao','nNF' ,Ide.nCFe));
          Ide.dEmi       := StrToDateDef(INIRec.ReadString( 'Identificacao','Emissao',INIRec.ReadString( 'Identificacao','dEmi',INIRec.ReadString( 'Identificacao','dhEmi',''))),Ide.dEmi);
          Ide.hEmi       := StrToTimeDef(INIRec.ReadString( 'Identificacao','hEmi',''),Ide.hEmi);
          Ide.cDV        := INIRec.ReadInteger( 'Identificacao','cDV' , Ide.cDV);
          Ide.tpAmb      := StrToTpAmb(OK,INIRec.ReadString( 'Identificacao','tpAmb',TpAmbToStr(Ide.tpAmb)));
          Ide.CNPJ       := INIRec.ReadString(  'Identificacao','CNPJ' ,edtSwHCNPJ.Text );
          Ide.signAC     := INIRec.ReadString(  'Identificacao','signAC' ,edtSwHAssinatura.Text );
          Ide.assinaturaQRCODE := INIRec.ReadString(  'Identificacao','assinaturaQRCODE' ,Ide.assinaturaQRCODE );
          Ide.numeroCaixa := INIRec.ReadInteger( 'Identificacao','numeroCaixa' , Ide.numeroCaixa);

          Emit.CNPJ              := INIRec.ReadString(  'Emitente','CNPJ'    ,INIRec.ReadString(  'Emitente','CNPJCPF', edtEmitCNPJ.Text ));
          Emit.xNome             := INIRec.ReadString(  'Emitente','Razao'   ,INIRec.ReadString(  'Emitente','xNome', Emit.xNome));
          Emit.xFant             := INIRec.ReadString(  'Emitente','Fantasia',INIRec.ReadString(  'Emitente','xFant', Emit.xFant));
          Emit.IE                := INIRec.ReadString(  'Emitente','IE', edtEmitIE.Text);
          Emit.IM                := INIRec.ReadString(  'Emitente','IM', edtEmitIM.Text);

          Emit.cRegTrib          := StrToRegTrib(      ok, INIRec.ReadString( 'Emitente','cRegTrib',      RegTribDescToStr(cbxRegTributario.ItemIndex)));
          Emit.cRegTribISSQN     := StrToRegTribISSQN( ok, INIRec.ReadString( 'Emitente','cRegTribISSQN', RegTribISSQNDescToStr(cbxRegTribISSQN.ItemIndex)));
          Emit.indRatISSQN       := StrToindRatISSQN(  ok, INIRec.ReadString( 'Emitente','indRatISSQN',   indRatISSQNDescToStr(cbxIndRatISSQN.ItemIndex)));

          Emit.EnderEmit.xLgr    := INIRec.ReadString(  'Emitente','Logradouro' ,INIRec.ReadString(  'Emitente','xLgr', Emit.EnderEmit.xLgr));
          Emit.EnderEmit.nro     := INIRec.ReadString(  'Emitente','Numero'     ,INIRec.ReadString(  'Emitente','nro', Emit.EnderEmit.nro));
          Emit.EnderEmit.xCpl    := INIRec.ReadString(  'Emitente','Complemento',INIRec.ReadString(  'Emitente','xCpl', Emit.EnderEmit.xCpl));
          Emit.EnderEmit.xBairro := INIRec.ReadString(  'Emitente','Bairro'     ,INIRec.ReadString(  'Emitente','xBairro', Emit.EnderEmit.xBairro));
          Emit.EnderEmit.xMun    := INIRec.ReadString(  'Emitente','Cidade'     ,INIRec.ReadString(  'Emitente','xMun', Emit.EnderEmit.xMun));
          Emit.EnderEmit.CEP     := INIRec.ReadInteger( 'Emitente','CEP', Emit.EnderEmit.CEP);

          Dest.CNPJCPF           := INIRec.ReadString(  'Destinatario','CNPJ'       ,INIRec.ReadString(  'Destinatario','CNPJCPF',INIRec.ReadString(  'Destinatario','CPF','')));
          Dest.xNome             := INIRec.ReadString(  'Destinatario','NomeRazao'  ,INIRec.ReadString(  'Destinatario','xNome'  ,''));

          if INIRec.ReadString(  'Entrega','xLgr','') <> '' then
           begin
             Entrega.xLgr    := INIRec.ReadString(  'Entrega','xLgr','');
             Entrega.nro     := INIRec.ReadString(  'Entrega','nro' ,'');
             Entrega.xCpl    := INIRec.ReadString(  'Entrega','xCpl','');
             Entrega.xBairro := INIRec.ReadString(  'Entrega','xBairro','');
             Entrega.xMun    := INIRec.ReadString(  'Entrega','xMun','');
             Entrega.UF      := INIRec.ReadString(  'Entrega','UF','');
           end;

          I := 1 ;

          while true do
           begin
             sSecao    := 'Produto'+IntToStrZero(I,3) ;
             sCodPro   := INIRec.ReadString(sSecao,'Codigo',INIRec.ReadString( sSecao,'cProd','FIM')) ;
             if sCodPro = 'FIM' then
                break ;

             with Det.Add do
              begin
                nItem := I;
                infAdProd      := INIRec.ReadString(sSecao,'infAdProd','');

                Prod.cProd    := INIRec.ReadString( sSecao,'Codigo'   ,INIRec.ReadString( sSecao,'cProd'   ,''));
                Prod.cEAN     := INIRec.ReadString( sSecao,'EAN'      ,INIRec.ReadString( sSecao,'cEAN'      ,''));
                Prod.xProd    := INIRec.ReadString( sSecao,'Descricao',INIRec.ReadString( sSecao,'xProd',''));
                Prod.NCM      := INIRec.ReadString( sSecao,'NCM'      ,'');
                Prod.CEST     := INIRec.ReadString( sSecao,'CEST'      ,'');
                Prod.CFOP     := INIRec.ReadString( sSecao,'CFOP'     ,'');
                Prod.uCom     := INIRec.ReadString( sSecao,'Unidade'  ,INIRec.ReadString( sSecao,'uCom'  ,''));
                Prod.EhCombustivel := (INIRec.ReadInteger( sSecao,'Combustivel',0)=1);
                Prod.qCom     := StringToFloatDef( INIRec.ReadString(sSecao,'Quantidade'   ,INIRec.ReadString(sSecao,'qCom'  ,'')) ,0) ;
                Prod.vUnCom   := StringToFloatDef( INIRec.ReadString(sSecao,'ValorUnitario',INIRec.ReadString(sSecao,'vUnCom','')) ,0) ;
                Prod.vProd    := StringToFloatDef( INIRec.ReadString(sSecao,'ValorTotal'   ,INIRec.ReadString(sSecao,'vProd' ,'')) ,0) ;
                Prod.indRegra := StrToindRegra(ok, INIRec.ReadString(sSecao,'indRegra','A'));
                Prod.vDesc    := StringToFloatDef( INIRec.ReadString(sSecao,'ValorDesconto',INIRec.ReadString(sSecao,'vDesc','')) ,0) ;
                Prod.vOutro   := StringToFloatDef( INIRec.ReadString(sSecao,'vOutro','') ,0) ;
                Prod.vItem    := StringToFloatDef( INIRec.ReadString(sSecao,'vItem','') ,0) ;
                Prod.vRatDesc := StringToFloatDef( INIRec.ReadString(sSecao,'vRatDesc','') ,0) ;
                Prod.vRatAcr  := StringToFloatDef( INIRec.ReadString(sSecao,'vRatAcr','') ,0) ;

                Imposto.vItem12741 := StringToFloatDef( INIRec.ReadString(sSecao,'vTotTrib',INIRec.ReadString(sSecao,'vItem12741','')) ,0) ;

                J := 1 ;
                while true do
                 begin
                   sSecao  := 'OBSFISCODET'+IntToStrZero(I,3)+IntToStrZero(J,3) ;
                   sFim    := INIRec.ReadString(sSecao,'xCampoDet','') ;
                   if (sFim <> '') then
                    begin
                      with Prod.obsFiscoDet.Add do
                       begin
                         xCampoDet := sFim;
                         xTextoDet := INIRec.ReadString(sSecao,'xTextoDet','') ; ;
                       end;
                    end
                   else
                      Break;
                   Inc(J);
                 end;

                with Imposto do
                 begin
                    sSecao := 'ICMS'+IntToStrZero(I,3) ;
                    sFim   := INIRec.ReadString( sSecao,'CST',INIRec.ReadString(sSecao,'CSOSN','FIM')) ;
                    if (sFim <> 'FIM') then
                     begin
                       with ICMS do
                       begin
                         ICMS.orig       := StrToOrig(     OK, INIRec.ReadString(sSecao,'Origem'    ,INIRec.ReadString(sSecao,'orig'    ,'0' ) ));
                         CST             := StrToCSTICMS(  OK, INIRec.ReadString(sSecao,'CST'       ,'00'));
                         CSOSN           := StrToCSOSNIcms(OK, INIRec.ReadString(sSecao,'CSOSN'     ,''  ));     //NFe2
                         ICMS.pICMS      := StringToFloatDef( INIRec.ReadString(sSecao,'Aliquota' ,INIRec.ReadString(sSecao,'pICMS','')) ,0);
                         ICMS.vICMS      := StringToFloatDef( INIRec.ReadString(sSecao,'Valor'    ,INIRec.ReadString(sSecao,'vICMS','')) ,0);
                       end;
                     end;

                    sSecao    := 'PIS'+IntToStrZero(I,3) ;
                    sFim   := INIRec.ReadString( sSecao,'CST','FIM') ;
                    if (sFim <> 'FIM') then
                     begin
                      with PIS do
                        begin
                         CST :=  StrToCSTPIS(OK, INIRec.ReadString( sSecao,'CST','01'));

                         PIS.vBC       := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'    ,INIRec.ReadString(sSecao,'vBC'      ,'')) ,0);
                         PIS.pPIS      := StringToFloatDef( INIRec.ReadString(sSecao,'Aliquota'     ,INIRec.ReadString(sSecao,'pPIS'     ,'')) ,0);
                         PIS.qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'Quantidade'   ,INIRec.ReadString(sSecao,'qBCProd'  ,'')) ,0);
                         PIS.vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'ValorAliquota',INIRec.ReadString(sSecao,'vAliqProd','')) ,0);
                         PIS.vPIS      := StringToFloatDef( INIRec.ReadString(sSecao,'Valor'        ,INIRec.ReadString(sSecao,'vPIS'     ,'')) ,0);
                        end;
                     end;

                    sSecao    := 'PISST'+IntToStrZero(I,3) ;
                    sFim   := INIRec.ReadString( sSecao,'ValorBase','F')+ INIRec.ReadString( sSecao,'Quantidade','IM') ;
                    if (sFim = 'FIM') then
                       sFim   := INIRec.ReadString( sSecao,'vBC','F')+ INIRec.ReadString( sSecao,'qBCProd','IM') ;

                    if (sFim <> 'FIM') then
                     begin
                      with PISST do
                       begin
                         vBc       := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'    ,INIRec.ReadString(sSecao,'vBC'      ,'')) ,0);
                         pPis      := StringToFloatDef( INIRec.ReadString(sSecao,'AliquotaPerc' ,INIRec.ReadString(sSecao,'pPis'     ,'')) ,0);
                         qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'Quantidade'   ,INIRec.ReadString(sSecao,'qBCProd'  ,'')) ,0);
                         vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'AliquotaValor',INIRec.ReadString(sSecao,'vAliqProd','')) ,0);
                         vPIS      := StringToFloatDef( INIRec.ReadString(sSecao,'ValorPISST'   ,INIRec.ReadString(sSecao,'vPIS'     ,'')) ,0);
                       end;
                     end;

                    sSecao    := 'COFINS'+IntToStrZero(I,3) ;
                    sFim   := INIRec.ReadString( sSecao,'CST','FIM') ;
                    if (sFim <> 'FIM') then
                     begin
                      with COFINS do
                       begin
                         CST := StrToCSTCOFINS(OK, INIRec.ReadString( sSecao,'CST','01'));

                         COFINS.vBC       := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'    ,INIRec.ReadString(sSecao,'vBC'      ,'')) ,0);
                         COFINS.pCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'Aliquota'     ,INIRec.ReadString(sSecao,'pCOFINS'  ,'')) ,0);
                         COFINS.qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'Quantidade'   ,INIRec.ReadString(sSecao,'qBCProd'  ,'')) ,0);
                         COFINS.vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'ValorAliquota',INIRec.ReadString(sSecao,'vAliqProd','')) ,0);
                         COFINS.vCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'Valor'        ,INIRec.ReadString(sSecao,'vCOFINS'  ,'')) ,0);
                       end;
                     end;

                    sSecao    := 'COFINSST'+IntToStrZero(I,3) ;
                    sFim   := INIRec.ReadString( sSecao,'ValorBase','F')+ INIRec.ReadString( sSecao,'Quantidade','IM');
                    if (sFim = 'FIM') then
                       sFim   := INIRec.ReadString( sSecao,'vBC','F')+ INIRec.ReadString( sSecao,'qBCProd','IM') ;

                    if (sFim <> 'FIM') then
                     begin
                      with COFINSST do
                       begin
                          vBC       := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'    ,INIRec.ReadString(sSecao,'vBC'      ,'')) ,0);
                          pCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'AliquotaPerc' ,INIRec.ReadString(sSecao,'pCOFINS'  ,'')) ,0);
                          qBCProd   := StringToFloatDef( INIRec.ReadString(sSecao,'Quantidade'   ,INIRec.ReadString(sSecao,'qBCProd'  ,'')) ,0);
                          vAliqProd := StringToFloatDef( INIRec.ReadString(sSecao,'AliquotaValor',INIRec.ReadString(sSecao,'vAliqProd','')) ,0);
                          vCOFINS   := StringToFloatDef( INIRec.ReadString(sSecao,'ValorCOFINSST',INIRec.ReadString(sSecao,'vCOFINS'  ,'')) ,0);
                        end;
                     end;

                    sSecao  := 'ISSQN'+IntToStrZero(I,3) ;
                    if INIRec.SectionExists(sSecao) then
                     begin
                      with ISSQN do
                       begin
                          vDeducISSQN := StringToFloatDef( INIRec.ReadString(sSecao,'vDeducISSQN','') ,0) ;
                          vBC       := StringToFloatDef( INIRec.ReadString(sSecao,'ValorBase'   ,INIRec.ReadString(sSecao,'vBC'   ,'')) ,0);
                          vAliq     := StringToFloatDef( INIRec.ReadString(sSecao,'Aliquota'    ,INIRec.ReadString(sSecao,'vAliq' ,'')) ,0);
                          vISSQN    := StringToFloatDef( INIRec.ReadString(sSecao,'ValorISSQN'  ,INIRec.ReadString(sSecao,'vISSQN','')) ,0);
                          cMunFG    := INIRec.ReadInteger(sSecao,'MunicipioFatoGerador', INIRec.ReadInteger(sSecao,'cMunFG',0));
                          cListServ := INIRec.ReadString(sSecao,'CodigoServico',INIRec.ReadString(sSecao,'cListServ',''));
                          cServTribMun := INIRec.ReadString(sSecao,'cServTribMun','');
                          cNatOp    := INIRec.ReadInteger(sSecao,'cNatOp',0);
                          indIncFisc:= StrToindIncentivo(OK,INIRec.ReadString(sSecao,'indIncFisc','0'));
                       end;
                     end;
                 end;

              end;
             Inc( I ) ;
           end ;

          Total.ICMSTot.vICMS   := StringToFloatDef( INIRec.ReadString('Total','ValorICMS'    ,INIRec.ReadString('Total','vICMS'   ,'')) ,0) ;
          Total.ICMSTot.vProd   := StringToFloatDef( INIRec.ReadString('Total','ValorProduto' ,INIRec.ReadString('Total','vProd'  ,'')) ,0) ;
          Total.ICMSTot.vDesc   := StringToFloatDef( INIRec.ReadString('Total','ValorDesconto',INIRec.ReadString('Total','vDesc'  ,'')) ,0) ;
          Total.ICMSTot.vPIS    := StringToFloatDef( INIRec.ReadString('Total','ValorPIS'     ,INIRec.ReadString('Total','vPIS'   ,'')) ,0) ;
          Total.ICMSTot.vCOFINS := StringToFloatDef( INIRec.ReadString('Total','ValorCOFINS'  ,INIRec.ReadString('Total','vCOFINS','')) ,0) ;
          Total.ICMSTot.vPISST  := StringToFloatDef( INIRec.ReadString('Total','ValorPISST'     ,INIRec.ReadString('Total','vPISST'   ,'')) ,0) ;
          Total.ICMSTot.vCOFINSST := StringToFloatDef( INIRec.ReadString('Total','ValorCOFINSST'  ,INIRec.ReadString('Total','vCOFINSST','')) ,0) ;
          Total.ICMSTot.vOutro  := StringToFloatDef( INIRec.ReadString('Total','ValorOutrasDespesas',INIRec.ReadString('Total','vOutro','')) ,0) ;

          Total.vCFe         := StringToFloatDef( INIRec.ReadString('Total','ValorNota'    ,INIRec.ReadString('Total','vCFe'    ,'')) ,0) ;
          Total.vCFeLei12741 := StringToFloatDef( INIRec.ReadString('Total','vTotTrib'     ,INIRec.ReadString('Total','vCFeLei12741'     ,'')),0) ;

          Total.ISSQNTot.vBC    := StringToFloatDef( INIRec.ReadString('Total','ValorBaseISS' ,INIRec.ReadString('ISSQNtot','vBC'  ,'')) ,0) ;
          Total.ISSQNTot.vISS   := StringToFloatDef( INIRec.ReadString('Total','ValorISSQN'   ,INIRec.ReadString('ISSQNtot','vISS' ,'')) ,0) ;
          Total.ISSQNTot.vPIS   := StringToFloatDef( INIRec.ReadString('Total','ValorPISISS'  ,INIRec.ReadString('ISSQNtot','vPIS' ,'')) ,0) ;
          Total.ISSQNTot.vCOFINS := StringToFloatDef( INIRec.ReadString('Total','ValorCONFINSISS',INIRec.ReadString('ISSQNtot','vCOFINS','')) ,0) ;
          Total.ISSQNTot.vPISST  := StringToFloatDef( INIRec.ReadString('Total','ValorPISISSST'  ,INIRec.ReadString('ISSQNtot','vPISST' ,'')) ,0) ;
          Total.ISSQNTot.vCOFINSST := StringToFloatDef( INIRec.ReadString('Total','ValorCONFINSISSST',INIRec.ReadString('ISSQNtot','vCOFINSST','')) ,0) ;

          Total.DescAcrEntr.vAcresSubtot := StringToFloatDef( INIRec.ReadString('Total','vAcresSubtot',INIRec.ReadString('DescAcrEntr','vAcresSubtot','')) ,0) ;
          Total.DescAcrEntr.vDescSubtot  := StringToFloatDef( INIRec.ReadString('Total','vDescSubtot',INIRec.ReadString('DescAcrEntr','vDescSubtot','')) ,0) ;

          Pagto.vTroco :=  StringToFloatDef( INIRec.ReadString('Total','vTroco','') ,0) ;

          I := 1 ;
          while true do
           begin
             sSecao    := 'pag'+IntToStrZero(I,3) ;
             sFim      := INIRec.ReadString(sSecao,'cMP','FIM');
             if (sFim = 'FIM') or (Length(sFim) <= 0) then
              begin
               sSecao    := 'Pagto'+IntToStrZero(I,3) ;
               sFim      := INIRec.ReadString(sSecao,'cMP','FIM');
               if (sFim = 'FIM') or (Length(sFim) <= 0) then
                 break ;
              end;

             with Pagto.Add do
              begin
                cMP  := StrToCodigoMP(OK,INIRec.ReadString(sSecao,'cMP',INIRec.ReadString(sSecao,'tpag','01')));
                vMP  := StringToFloatDef( INIRec.ReadString(sSecao,'vMP',INIRec.ReadString(sSecao,'vPag','')) ,0) ;
                cAdmC  := INIRec.ReadInteger(sSecao,'cAdmC',0);
              end;
             Inc(I);
           end;

          InfAdic.infCpl     :=  INIRec.ReadString( 'DadosAdicionais','Complemento',INIRec.ReadString( 'DadosAdicionais','infCpl'    ,''));

          I := 1 ;
          while true do
           begin
             sSecao := 'ObsFisco'+IntToStrZero(I,3) ;
             sFim   := INIRec.ReadString(sSecao,'Campo',INIRec.ReadString(sSecao,'xCampo','FIM')) ;
             if (sFim = 'FIM') or (Length(sFim) <= 0) then
                break ;

             with InfAdic.obsFisco.Add do
              begin
                xCampo := sFim;
                xTexto := INIRec.ReadString( sSecao,'Texto',INIRec.ReadString( sSecao,'xTexto',''));
              end;
             Inc(I);
           end;
         end;
         ACBrSAT1.CFe.GerarXML(ApenasTagsAplicacao);
     end;
  finally
    INIRec.Free;
  end;
end;

end.
