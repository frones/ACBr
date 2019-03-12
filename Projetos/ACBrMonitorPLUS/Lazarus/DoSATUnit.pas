unit DoSATUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CmdUnit, pcnConversao, strutils,
  ACBrSAT, ACBrMonitorConfig, ACBrMonitorConsts, ACBrDFeUtil,
  ACBrLibSATRespostas, ACBrLibResposta,
  ACBrSATExtratoESCPOS, ACBrSATExtratoFortesFr, ACBrValidador, ACBrDFeSSL;

type

{ TACBrObjetoSAT }

TACBrObjetoSAT = class(TACBrObjetoDFe)
private
  fACBrSAT: TACBrSAT;
public
  constructor Create(AConfig: TMonitorConfig; ACBrSAT: TACBrSAT); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  procedure RespostaConsultaSessao(ArqCFe: String);
  procedure RespostaConsultaSessaoCancelado(ArqCFe: String);
  procedure RespostaStatusSAT;
  procedure RespostaCriarCFe(ArqCFe: String);
  procedure RespostaEnviarDadosVenda( Resultado: String);
  procedure RespostaCancelarVenda( Resultado: String);
  procedure RespostaTesteFimaFim( Resultado: String );
  procedure RespostaPadrao;
  procedure RespostaIntegrador;

  procedure CarregarDadosVenda(aStr: String; aNomePDF : String = '');
  procedure CarregarDadosCancelamento(aStr: String);
  function ParamAsXML(AParam: String): String;

  procedure GerarIniCFe( AStr: WideString; ApenasTagsAplicacao: Boolean = True);

  property ACBrSAT: TACBrSAT read fACBrSAT;
end;

{ TMetodoAtivar }

TMetodoAtivar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoInicializar }

TMetodoInicializar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDesinicializar}

TMetodoDesinicializar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAssociarAssinatura}

TMetodoAssociarAssinatura = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBloquear}

TMetodoBloquear = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDesbloquear}

TMetodoDesbloquear = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTrocarCodigoAtivacao}

TMetodoTrocarCodigoAtivacao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarSat }

TMetodoConsultarSat = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarStatusOperacional }

TMetodoConsultarStatusOperacional = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarSessao }

TMetodoConsultarSessao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAtualizaSoftware }

TMetodoAtualizaSoftware = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoComunicarCertificado }

TMetodoComunicarCertificado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCarregarDadosVenda }

TMetodoCarregarDadosVenda = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCarregarDadosCancelamento }

TMetodoCarregarDadosCancelamento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarCFe }

TMetodoCriarCFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCriarEnviarCFe }

TMetodoCriarEnviarCFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarCFe }

TMetodoEnviarCFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCancelarCFe }

TMetodoCancelarCFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirExtratoVenda }

TMetodoImprimirExtratoVenda = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirExtratoResumido }

TMetodoImprimirExtratoResumido = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimirExtratoCancelamento }

TMetodoImprimirExtratoCancelamento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGerarImpressaoFiscalMFe }

TMetodoGerarImpressaoFiscalMFe = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGerarPDFExtratoVenda }

TMetodoGerarPDFExtratoVenda = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoExtrairLog }

TMetodoExtrairLog = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTesteFimaFim}

TMetodoTesteFimaFim = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetNumeroSessao}

TMetodoSetNumeroSessao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetLogoMarca}

TMetodoSetLogoMarca = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGerarAssinaturaSAT }

TMetodoGerarAssinaturaSAT = class(TACBrMetodo)
public
  procedure Executar; override;
end;


implementation

uses
  ACBrUtil,DoACBrUnit,IniFiles, pcnAuxiliar, typinfo,
  ACBrSATExtratoClass;


procedure TACBrObjetoSAT.CarregarDadosVenda(aStr: String; aNomePDF: String);
begin
  if Trim(aStr) = '' then
    exit;

  with fACBrSAT do
  begin
    CFe.Clear;
    if (pos(#10,aStr) = 0) and FileExists(aStr) then
      CFe.LoadFromFile(aStr)
    else
      CFe.AsXMLString := ConvertStrRecived(aStr);

    if ( Extrato.Filtro = TACBrSATExtratoFiltro(fiPDF) ) then
      Extrato.NomeDocumento := IfThen(aNomePDF <> '', aNomePDF ,
        CalcCFeNomeArq(ConfigArquivos.PastaCFeVenda,CFe.infCFe.ID,'','.pdf'));
  end;

end;

function TACBrObjetoSAT.ParamAsXML(AParam: String): String;
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

procedure TACBrObjetoSAT.CarregarDadosCancelamento(aStr: String);
begin
  if Trim(aStr) = '' then
    exit;

  if (pos(#10,aStr) = 0) and FileExists(aStr) then
    fACBrSAT.CFeCanc.LoadFromFile(aStr)
  else
    fACBrSAT.CFeCanc.AsXMLString := ConvertStrRecived(aStr);

end;

procedure TACBrObjetoSAT.GerarIniCFe(AStr: WideString; ApenasTagsAplicacao: Boolean = True);
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
    with fACBrSAT do
     begin
       ACBrSAT.InicializaCFe;

       with ACBrSAT.CFe do
        begin

          ACBrSAT.Config.infCFe_versaoDadosEnt := StringToFloatDef( INIRec.ReadString('infCFe','versao',''),ACBrSAT.Config.infCFe_versaoDadosEnt) ;

          infCFe.versaoDadosEnt := ACBrSAT.Config.infCFe_versaoDadosEnt;

          Ide.cUF        := INIRec.ReadInteger( 'Identificacao','cUF' ,UFparaCodigo(INIRec.ReadString(  'Emitente','UF', CodigoParaUF(Ide.cUF))));
          Ide.cNF        := INIRec.ReadInteger( 'Identificacao','Codigo' ,INIRec.ReadInteger( 'Identificacao','cNF' ,Ide.cNF));
          Ide.modelo     := INIRec.ReadInteger( 'Identificacao','Modelo' ,INIRec.ReadInteger( 'Identificacao','mod' ,Ide.modelo));
          Ide.nserieSAT  := INIRec.ReadInteger( 'Identificacao','nserieSAT'  ,Ide.nserieSAT);
          Ide.nCFe       := INIRec.ReadInteger( 'Identificacao','nCFe' ,INIRec.ReadInteger( 'Identificacao','nNF' ,Ide.nCFe));
          Ide.dEmi       := StrToDateDef(INIRec.ReadString( 'Identificacao','Emissao',INIRec.ReadString( 'Identificacao','dEmi',INIRec.ReadString( 'Identificacao','dhEmi',''))),Ide.dEmi);
          Ide.hEmi       := StrToTimeDef(INIRec.ReadString( 'Identificacao','hEmi',''),Ide.hEmi);
          Ide.cDV        := INIRec.ReadInteger( 'Identificacao','cDV' , Ide.cDV);
          Ide.tpAmb      := StrToTpAmb(OK,INIRec.ReadString( 'Identificacao','tpAmb',TpAmbToStr(Ide.tpAmb)));
          Ide.CNPJ       := INIRec.ReadString(  'Identificacao','CNPJ' , MonitorConfig.SAT.SATSWH.CNPJ );
          Ide.signAC     := INIRec.ReadString(  'Identificacao','signAC' ,MonitorConfig.SAT.SATSWH.Assinatura );
          Ide.assinaturaQRCODE := INIRec.ReadString(  'Identificacao','assinaturaQRCODE' ,Ide.assinaturaQRCODE );
          Ide.numeroCaixa := INIRec.ReadInteger( 'Identificacao','numeroCaixa' , Ide.numeroCaixa);

          Emit.CNPJ              := INIRec.ReadString(  'Emitente','CNPJ'    ,INIRec.ReadString(  'Emitente','CNPJCPF', MonitorConfig.SAT.SATImpressao.SATEmit.CNPJ ));
          Emit.xNome             := INIRec.ReadString(  'Emitente','Razao'   ,INIRec.ReadString(  'Emitente','xNome', Emit.xNome));
          Emit.xFant             := INIRec.ReadString(  'Emitente','Fantasia',INIRec.ReadString(  'Emitente','xFant', Emit.xFant));
          Emit.IE                := INIRec.ReadString(  'Emitente','IE', MonitorConfig.SAT.SATImpressao.SATEmit.IE);
          Emit.IM                := INIRec.ReadString(  'Emitente','IM', MonitorConfig.SAT.SATImpressao.SATEmit.IM);

          Emit.cRegTrib          := StrToRegTrib(      ok, INIRec.ReadString( 'Emitente','cRegTrib',      RegTribDescToStr(MonitorConfig.SAT.SATImpressao.SATEmit.RegTributario)));
          Emit.cRegTribISSQN     := StrToRegTribISSQN( ok, INIRec.ReadString( 'Emitente','cRegTribISSQN', RegTribISSQNDescToStr(MonitorConfig.SAT.SATImpressao.SATEmit.RegTribISSQN)));
          Emit.indRatISSQN       := StrToindRatISSQN(  ok, INIRec.ReadString( 'Emitente','indRatISSQN',   indRatISSQNDescToStr(MonitorConfig.SAT.SATImpressao.SATEmit.IndRatISSQN)));

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

             with Det.New do
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
                      with Prod.obsFiscoDet.New do
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

             with Pagto.New do
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

             with InfAdic.obsFisco.New do
              begin
                xCampo := sFim;
                xTexto := INIRec.ReadString( sSecao,'Texto',INIRec.ReadString( sSecao,'xTexto',''));
              end;
             Inc(I);
           end;
         end;
         ACBrSAT.CFe.GerarXML(ApenasTagsAplicacao);
     end;
  finally
    INIRec.Free;
  end;
end;

{ TMetodoGerarAssinaturaSAT }

{ Params: 0 - CNPJSHW : cnpj da Software House
          1 - CNPJEmitente : cnpj do Emitente
}
procedure TMetodoGerarAssinaturaSAT.Executar;
var
  cCNPJShw: String;
  cCNPJEmitente: String;
  cCodigoVinculacao: String;
  cMsgErroValidacao: String;

  procedure ConfiguraDFe;
  begin
    with TACBrObjetoSAT(fpObjetoDono) do
    begin
      case MonitorConfig.DFE.Certificado.CryptLib of
        1: fACBrSAT.SSL.SSLCryptLib := cryOpenSSL;
        2: fACBrSAT.SSL.SSLCryptLib := cryCapicom;
        3: fACBrSAT.SSL.SSLCryptLib := cryWinCrypt;
      else
        fACBrSAT.SSL.SSLCryptLib := cryWinCrypt;
      end;

      if NaoEstaVazio(MonitorConfig.DFE.Certificado.ArquivoPFX) then
        fACBrSAT.SSL.ArquivoPFX  := Trim(MonitorConfig.DFE.Certificado.ArquivoPFX )
      else
        fACBrSAT.SSL.NumeroSerie := Trim(MonitorConfig.DFE.Certificado.NumeroSerie );

      fACBrSAT.SSL.Senha       := Trim(MonitorConfig.DFE.Certificado.Senha );

    end;

  end;

begin
  cCNPJShw:= fpCmd.Params(0);
  cCNPJEmitente:= fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin

    if EstaVazio(cCNPJShw) then
      cCNPJShw:= Trim(MonitorConfig.SAT.SATSWH.CNPJ);

    if EstaVazio(cCNPJEmitente) then
      cCNPJEmitente:= Trim(MonitorConfig.SAT.SATImpressao.SATEmit.CNPJ);

    if EstaVazio(Trim(MonitorConfig.DFE.Certificado.NumeroSerie )) and
       EstaVazio(Trim(MonitorConfig.DFE.Certificado.ArquivoPFX ))then
      raise Exception.Create('Certificado não foi informado!');


    cMsgErroValidacao := ACBrValidador.ValidarCNPJ(cCNPJShw);
    if NaoEstaVazio( Trim(cMsgErroValidacao) ) then
      raise Exception.Create('CNPJ da Software House inválido!');


    cMsgErroValidacao := ACBrValidador.ValidarCNPJ(cCNPJEmitente);
    if Trim(cMsgErroValidacao) <> '' then
      raise Exception.Create('CNPJ do Emitente inválido!');

    ConfiguraDFe;

    cCodigoVinculacao := Onlynumber(cCNPJShw) + Onlynumber(cCNPJEmitente);
    fpCmd.Resposta := fACBrSAT.SSL.CalcHash(cCodigoVinculacao, dgstSHA256, outBase64, True);

  end;

end;

{ TMetodoSetLogoMarca }

{ Params: 0 - cpath: path do logo
}
procedure TMetodoSetLogoMarca.Executar;
var
  cPath : String;
begin
  cPath := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if FileExists(cPath) then
    begin
      TACBrSATExtratoFortes(ACBrSAT.Extrato).LogoVisible := True;
      TACBrSATExtratoFortes(ACBrSAT.Extrato).PictureLogo.LoadFromFile(cPath);
      with MonitorConfig.DFE do
        Impressao.Geral.LogoMarcaNFCeSAT:= cPath;
      MonitorConfig.SalvarArquivo;

    end
    else
      raise Exception.Create('Arquivo não encontrado.');
  end;

end;

{ TMetodoSetNumeroSessao }

{ Params: 0 - cNumero: Numero de sessão
}
procedure TMetodoSetNumeroSessao.Executar;
var
  cNumero : String;
begin
  cNumero := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
    ACBrSAT.Tag := StrToIntDef(Trim(cNumero), 0);

end;

{ TMetodoTesteFimaFim }

{ Params: 0 - cXMLVenda: String com caminho oo Arquivo XML de Venda
}
procedure TMetodoTesteFimaFim.Executar;
var
  cXMLVenda : String;
  Resultado : String;
begin
  cXMLVenda := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    ACBrSAT.InicializaCFe;
    CarregarDadosVenda(cXMLVenda);
    Resultado := ACBrSAT.TesteFimAFim(ACBrSAT.CFe.GerarXML(True));

    RespostaTesteFimaFim(Resultado);

  end;

end;

{ TMetodoExtrairLog }

{ Params: 0 - cPathArq: String com caminho do Arquivo
}
procedure TMetodoExtrairLog.Executar;
var
  cPathArq : String;
begin
  cPathArq := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
    ACBrSAT.ExtrairLogs(cPathArq);

end;

{ TMetodoGerarPDFExtratoVenda }

{ Params: 0 - cXMLVenda: String com XML de Vendas o path do arquivo
          1 - cNomeArq: String com nome do o Arquivo
}
procedure TMetodoGerarPDFExtratoVenda.Executar;
var
  cXMLVenda : String;
  cNomeArq : String;
begin
  cXMLVenda := fpCmd.Params(0);
  cNomeArq := fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    DoPrepararImpressaoSAT(cXMLVenda, True);
    CarregarDadosVenda(cXMLVenda, cNomeArq);
    ACBrSAT.ImprimirExtrato;

    RespostaPadrao;

  end;

end;

{ TMetodoGerarImpressaoFiscalMFe }

{ Params: 0 - cXMLVenda: String com XML de Vendas o path do arquivo
          1 - cImpressora: String com nome da impressora
}
procedure TMetodoGerarImpressaoFiscalMFe.Executar;
var
  cXMLVenda : String;
  cImpressora : String;
begin
  cXMLVenda := fpCmd.Params(0);
  cImpressora := fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    DoPrepararImpressaoSAT(cImpressora);
    CarregarDadosVenda(cXMLVenda);
    TACBrSATExtratoESCPOS(ACBrSAT.Extrato).GerarImpressaoFiscalMFe();

  end;

end;


{ TMetodoImprimirExtratoCancelamento }

{ Params: 0 - cXMLVenda: String com XML de Vendas o path do arquivo
          1 - cXMLCancelamento: String com XML de Cancelamentoo path do arquivo
          2 - cImpressora: String com nome da impressora
}
procedure TMetodoImprimirExtratoCancelamento.Executar;
var
  cXMLVenda : String;
  cXMLCancelamento : String;
  cImpressora : String;
begin
  cXMLVenda := fpCmd.Params(0);
  cXMLCancelamento := fpCmd.Params(1);
  cImpressora := fpCmd.Params(2);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    DoPrepararImpressaoSAT(cImpressora);
    CarregarDadosVenda(cXMLVenda);
    CarregarDadosCancelamento(cXMLCancelamento);
    ACBrSAT.ImprimirExtratoCancelamento;
  end;

end;


{ TMetodoImprimirExtratoResumido }

{ Params: 0 - cXMLVenda: String com XML de Vendas o path do arquivo
          1 - cImpressora: String com nome da impressora
}
procedure TMetodoImprimirExtratoResumido.Executar;
var
  cXMLVenda : String;
  cImpressora : String;
begin
  cXMLVenda := fpCmd.Params(0);
  cImpressora := fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    DoPrepararImpressaoSAT(cImpressora);
    CarregarDadosVenda(cXMLVenda);
    ACBrSAT.ImprimirExtratoResumido;
  end;

end;

{ TMetodoImprimirExtratoVenda }

{ Params: 0 - cXMLVenda: String com XML de Vendas o path do arquivo
          1 - cImpressora: String com nome da impressora
}
procedure TMetodoImprimirExtratoVenda.Executar;
var
  cXMLVenda : String;
  cImpressora : String;
begin
  cXMLVenda := fpCmd.Params(0);
  cImpressora := fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    DoPrepararImpressaoSAT(cImpressora);
    CarregarDadosVenda(cXMLVenda);
    ACBrSAT.ImprimirExtrato;
  end;

end;

{ TMetodoCancelarCFe }

{ Params: 0 - cArqXMLVenda: String com XML de Venda
}
procedure TMetodoCancelarCFe.Executar;
var
  cArqXMLVenda, Resultado : String;
begin
  cArqXMLVenda := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if NaoEstaVazio(cArqXMLVenda) then
      CarregarDadosVenda(cArqXMLVenda);

    Resultado := ACBrSAT.CancelarUltimaVenda;

    RespostaCancelarVenda(Resultado);

  end;

end;

{ TMetodoEnviarCFe }

{ Params: 0 - cArqXML: String com XML ou Path do arquivo
}
procedure TMetodoEnviarCFe.Executar;
var
  cArqXML, ArqCFe, Resultado: String;
begin
  cArqXML := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin

    ArqCFe := ParamAsXML(cArqXML);
    if ArqCFe = '' then
      Resultado := ACBrSAT.EnviarDadosVenda
    else
      Resultado := ACBrSAT.EnviarDadosVenda( ArqCFe );

    RespostaEnviarDadosVenda( Resultado );

  end;

end;

{ TMetodoCriarEnviarCFe }

{ Params: 0 - cArqIni: String com INI ou Path do arquivo
}
procedure TMetodoCriarEnviarCFe.Executar;
var
  cArqIni, Resultado: String;
begin
  cArqIni := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    GerarIniCFe( cArqIni, True );

    Resultado := ACBrSAT.EnviarDadosVenda( ACBrSAT.CFe.AsXMLString );
    RespostaEnviarDadosVenda( Resultado );

  end;

end;

{ TMetodoCriarCFe }

{ Params: 0 - cArqIni: String com INI ou Path do arquivo
}
procedure TMetodoCriarCFe.Executar;
var
  cArqIni, ArqCFe: String;
begin
  cArqIni := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    GerarIniCFe( cArqIni, False );

    ArqCFe := '';
    ACBrSAT.CFe.GerarXML( True ); // Tags da Aplicação
    if MonitorConfig.SAT.SalvarCFe then
    begin
      ArqCFe := ACBrSAT.CalcCFeNomeArq(ACBrSAT.ConfigArquivos.PastaEnvio,
                          IntToStrZero(ACBrSAT.CFe.ide.numeroCaixa,3)+'-'+
                          IntToStrZero(ACBrSAT.CFe.ide.cNF,6),'-satcfe');
      ACBrSAT.CFe.SaveToFile(ArqCFe);
    end;

    RespostaCriarCFe(ArqCFe);

  end;

end;

{ TMetodoCarregarDadosCancelamento }

{ Params: 0 - aStr: Path do arquivo
}
procedure TMetodoCarregarDadosCancelamento.Executar;
var
  aStr : String;
begin
  aStr := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
    CarregarDadosCancelamento(aStr);

end;

{ TMetodoCarregarDadosVenda }

{ Params: 0 - aStr: Path do arquivo
}
procedure TMetodoCarregarDadosVenda.Executar;
var
  aStr : String;
begin
  aStr := fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
   CarregarDadosVenda(aStr);

end;

{ TMetodoComunicarCertificado }

{ Params: 0 - cNumeroCertificado
}
procedure TMetodoComunicarCertificado.Executar;
var
  cNumeroCertificado: AnsiString;
begin
  cNumeroCertificado     :=  fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrSAT.ComunicarCertificadoICPBRASIL(cNumeroCertificado);
  end;

end;

{ TMetodoAtualizaSoftware }

procedure TMetodoAtualizaSoftware.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
    fpCmd.Resposta := ACBrSAT.AtualizarSoftwareSAT;


end;

{ TMetodoConsultarSessao }

{ Params: 0 - cConsultarSessao
}
procedure TMetodoConsultarSessao.Executar;
var
  cConsultarSessao: String;
begin
  cConsultarSessao     :=  fpCmd.Params(0);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    ACBrSAT.CFe.Clear;
    ACBrSAT.CFeCanc.Clear;

    fpCmd.Resposta := ACBrSAT.ConsultarNumeroSessao(StrToInt(cConsultarSessao));

    if ACBrSAT.Resposta.codigoDeRetorno = 6000 then
      RespostaConsultaSessao(ACBrSAT.CFe.NomeArquivo);

    if ACBrSAT.Resposta.codigoDeRetorno = 7000 then
      RespostaConsultaSessaoCancelado(ACBrSAT.CFeCanc.NomeArquivo);

  end;

end;

{ TMetodoConsultarStatusOperacional }

procedure TMetodoConsultarStatusOperacional.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrSAT.ConsultarStatusOperacional;

    if (ACBrSAT.Resposta.codigoDeRetorno = 10000) then
      RespostaStatusSAT;

  end;

end;

{ TMetodoConsultarSat }

procedure TMetodoConsultarSat.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
    fpCmd.Resposta := ACBrSAT.ConsultarSAT;

end;

{ TMetodoTrocarCodigoAtivacao }

{ Params: 0 - cCodAtivacao - Uma String com código para ativação
          1 - cOpcao - Uma String com a assinatura
          2 - cNovoCodAtivacao - Uma String com novo código para ativação
}
procedure TMetodoTrocarCodigoAtivacao.Executar;
var
cCodAtivacao, cOpcao, cNovoCodAtivacao: String;
begin
  cCodAtivacao     :=  fpCmd.Params(0);
  cOpcao           :=  fpCmd.Params(1);
  cNovoCodAtivacao :=  fpCmd.Params(2);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrSAT.TrocarCodigoDeAtivacao(cCodAtivacao,
                               StrToIntDef(cOpcao,1), cNovoCodAtivacao)
  end;

end;

{ TMetodoDesbloquear }

procedure TMetodoDesbloquear.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
    fpCmd.Resposta := ACBrSAT.DesbloquearSAT;
end;

{ TMetodoBloquear }

{ Params:
}
procedure TMetodoBloquear.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
    fpCmd.Resposta := ACBrSAT.BloquearSAT;
end;

{ TMetodoAssociarAssinatura }

{ Params: 0 - CNPJs - Uma String contendo o CNPJ da Sw.House + CNPJ do Emissor para ativação
          1 - Assinatura - Uma String com a assinatura
}
procedure TMetodoAssociarAssinatura.Executar;
var
  cCNPJs, cCNPJSwHouse, cCNPJEmissor, cAssinatura: String;
begin
  cCNPJs := fpCmd.Params(0);
  cCNPJSwHouse := copy(cCNPJs, 1,14);
  cCNPJEmissor := copy(cCNPJs,15,14);
  cAssinatura := fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono), MonitorConfig do
  begin
    if EstaVazio(Trim(cCNPJSwHouse)) then
      cCNPJSwHouse := SAT.SATSWH.CNPJ;

    if EstaVazio(Trim(cCNPJEmissor)) then
      cCNPJEmissor := SAT.SATImpressao.SATEmit.CNPJ;

    if EstaVazio(Trim(cAssinatura)) then
      cAssinatura := SAT.SATSWH.Assinatura;

    if (ACBrSAT.Config.ide_tpAmb <> taHomologacao) then
    begin
       if (not ValidarCNPJ(cCNPJSwHouse)) then
         raise Exception.Create('CNPJ Sw.House inválido: '+cCNPJSwHouse);

       if (not ValidarCNPJ(cCNPJEmissor)) then
         raise Exception.Create('CNPJ Emissor inválido: '+cCNPJEmissor);
    end;

    fpCmd.Resposta := ACBrSAT.AssociarAssinatura(cCNPJSwHouse + cCNPJEmissor, cAssinatura);
  end;
end;

{ TMetodoDesinicializar }

procedure TMetodoDesinicializar.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if not ACBrSAT.Inicializado then
      fpCmd.Resposta := 'SAT não inicializado'
    else
    begin
      ACBrSAT.DesInicializar;
      fpCmd.Resposta := 'SAT desinicializado'
    end;
  end;

end;

{ TMetodoInicializar }

{ Params:
}
procedure TMetodoInicializar.Executar;
begin
  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if ACBrSAT.Inicializado then
       fpCmd.Resposta := 'SAT ja inicializado'
    else
    begin
       ACBrSAT.Inicializar;
       fpCmd.Resposta := 'SAT inicializado';
    end;
  end;

end;

{ TMetodoAtivar }


{ Params: 0 - CNPJ - Uma String com CNPJ para ativação
          1 - UF - Uma String com código da UF para ativação
}
procedure TMetodoAtivar.Executar;
var
  cCNPJ, cUF: String;
begin
  cCNPJ := fpCmd.Params(0);
  cUF :=   fpCmd.Params(1);

  with TACBrObjetoSAT(fpObjetoDono) do
  begin
    if (EstaVazio(Trim(cCNPJ)) and
          EstaVazio(Trim(cUF))) then
        with MonitorConfig.SAT do
          fpCmd.Resposta := ACBrSAT.AtivarSAT(1, OnlyNumber(SATImpressao.SATEmit.CNPJ), StrToInt(CodigoUF))
      else
      begin
        if (ACBrSAT.Config.ide_tpAmb <> taHomologacao) and
           (not ValidarCNPJ(cCNPJ)) then
          raise Exception.Create('CNPJ '+cCNPJ+' inválido.');

        fpCmd.Resposta := ACBrStr( ACBrSAT.AtivarSAT(1,cCNPJ, StrToInt(cUF)) );
      end;

    end;

end;

{ TACBrObjetoSAT }

constructor TACBrObjetoSAT.Create(AConfig: TMonitorConfig; ACBrSAT: TACBrSAT);
begin
  inherited Create(AConfig);

  fACBrSAT := ACBrSAT;

  ListaDeMetodos.Add(CMetodoSATAtivar);
  ListaDeMetodos.Add(CMetodoInicializar);
  ListaDeMetodos.Add(CMetodoDesInicializar);
  ListaDeMetodos.Add(CMetodoAssociarAssinatura);
  ListaDeMetodos.Add(CMetodoBloquear);
  ListaDeMetodos.Add(CMetodoDesbloquear);
  ListaDeMetodos.Add(CMetodotrocarcodigoativacao);
  ListaDeMetodos.Add(CMetodoConsultarSat);
  ListaDeMetodos.Add(CMetodoConsultarStatusOperacional);
  ListaDeMetodos.Add(CMetodoConsultarSessao);
  ListaDeMetodos.Add(CMetodoConsultarNumeroSessao);
  ListaDeMetodos.Add(CMetodoAtualizaSoftware);
  ListaDeMetodos.Add(CMetodoAtualizarSoftwareSAT);
  ListaDeMetodos.Add(CMetodoComunicarCertificado);
  ListaDeMetodos.Add(CMetodoComunicarCertificadoICPBrasil);
  ListaDeMetodos.Add(CMetodoCarregarDadosVenda);
  ListaDeMetodos.Add(CMetodoCarregarDadosCancelamento);
  ListaDeMetodos.Add(CMetodoCriarCFe);
  ListaDeMetodos.Add(CMetodoCriarEnviarCFe);
  ListaDeMetodos.Add(CMetodoEnviarCFe);
  ListaDeMetodos.Add(CMetodoCancelarCFe);
  ListaDeMetodos.Add(CMetodoImprimirExtratoVenda);
  ListaDeMetodos.Add(CMetodoImprimirExtratoResumido);
  ListaDeMetodos.Add(CMetodoImprimirExtratoCancelamento);
  ListaDeMetodos.Add(CMetodoGerarImpressaoFiscalMFe);
  ListaDeMetodos.Add(CMetodoExtrairLogs);
  ListaDeMetodos.Add(CMetodoTesteFimaFim);
  ListaDeMetodos.Add(CMetodoGerarPDFExtratoVenda);
  ListaDeMetodos.Add(CMetodoSetNumeroSessao);
  ListaDeMetodos.Add(CMetodoSetlogomarcaSAT);
  ListaDeMetodos.Add(CMetodoGerarAssinaturaSAT);

  // DoACBr
  ListaDeMetodos.Add(CMetodoSavetofile);
  ListaDeMetodos.Add(CMetodoLoadfromfile);
  ListaDeMetodos.Add(CMetodoLerini);
  ListaDeMetodos.Add(CMetodoSetcertificado);
  ListaDeMetodos.Add(CMetodoRestaurar);
  ListaDeMetodos.Add(CMetodoOcultar);
  ListaDeMetodos.Add(CMetodoEncerrarmonitor);
  ListaDeMetodos.Add(CMetodoAtivo);
  ListaDeMetodos.Add(CMetodoDatahora);
  ListaDeMetodos.Add(CMetodoData);
  ListaDeMetodos.Add(CMetodoHora);
  ListaDeMetodos.Add(CMetodoExit);
  ListaDeMetodos.Add(CMetodoBye);
  ListaDeMetodos.Add(CMetodoFim);
  ListaDeMetodos.Add(CMetodoSair);

end;

procedure TACBrObjetoSAT.Executar(ACmd: TACBrCmd);
var
  AMetodoClass: TACBrMetodoClass;
  CmdNum: Integer;
  Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoAtivar;
    1  : AMetodoClass := TMetodoInicializar;
    2  : AMetodoClass := TMetodoDesinicializar;
    3  : AMetodoClass := TMetodoAssociarAssinatura;
    4  : AMetodoClass := TMetodoBloquear;
    5  : AMetodoClass := TMetodoDesbloquear;
    6  : AMetodoClass := TMetodoTrocarCodigoAtivacao;
    7  : AMetodoClass := TMetodoConsultarSat;
    8  : AMetodoClass := TMetodoConsultarStatusOperacional;
    9  : AMetodoClass := TMetodoConsultarSessao;
    10 : AMetodoClass := TMetodoConsultarSessao;
    11 : AMetodoClass := TMetodoAtualizaSoftware;
    12 : AMetodoClass := TMetodoAtualizaSoftware;
    13 : AMetodoClass := TMetodoComunicarCertificado;
    14 : AMetodoClass := TMetodoComunicarCertificado;
    15 : AMetodoClass := TMetodoCarregarDadosVenda;
    16 : AMetodoClass := TMetodoCarregarDadosCancelamento;
    17 : AMetodoClass := TMetodoCriarCFe;
    18 : AMetodoClass := TMetodoCriarEnviarCFe;
    19 : AMetodoClass := TMetodoEnviarCFe;
    20 : AMetodoClass := TMetodoCancelarCFe;
    21 : AMetodoClass := TMetodoImprimirExtratoVenda;
    22 : AMetodoClass := TMetodoImprimirExtratoResumido;
    23 : AMetodoClass := TMetodoImprimirExtratoCancelamento;
    24 : AMetodoClass := TMetodoGerarImpressaoFiscalMFe;
    25 : AMetodoClass := TMetodoExtrairLog;
    26 : AMetodoClass := TMetodoTesteFimaFim;
    27 : AMetodoClass := TMetodoGerarPDFExtratoVenda;
    28 : AMetodoClass := TMetodoSetNumeroSessao;
    29 : AMetodoClass := TMetodoSetLogoMarca;
    30 : AMetodoClass := TMetodoGerarAssinaturaSAT;

    31..46 : DoACbr(ACmd);
  end;

  if Assigned(AMetodoClass) then
  begin
    Ametodo := AMetodoClass.Create(ACmd, Self);
    try
      Ametodo.Executar;
    finally
      Ametodo.Free;
    end;
  end;

end;

procedure TACBrObjetoSAT.RespostaConsultaSessao(ArqCFe: String);
var
  Resp: TRetornoConsultarSessao;
begin
  Resp := TRetornoConsultarSessao.Create(resINI);
  try
    with fACBrSAT.CFe do
    begin
      Resp.nCFe := IntToStrZero(ide.nCFe,0);
      Resp.XML  := AsXMLString;
      Resp.Arquivo:= ArqCFe;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;
    end;
  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaConsultaSessaoCancelado(ArqCFe: String);
var
  Resp: TRetornoConsultarSessaoCancelado;
begin
  Resp := TRetornoConsultarSessaoCancelado.Create(resINI);
  try
    with fACBrSAT.CFeCanc do
    begin
      Resp.nCFeCanc := IntToStrZero(ide.nCFe,0);
      Resp.XML  := AsXMLString;
      Resp.Arquivo:= ArqCFe;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;

    end;
  finally
    Resp.Free;
  end;
end;

procedure TACBrObjetoSAT.RespostaStatusSAT;
var
  Resp: TRetornoStatusSAT;
begin
  Resp := TRetornoStatusSAT.Create(resINI);
  try
    with fACBrSAT.Status do
    begin
      Resp.NSERIE :=            NSERIE;
      Resp.LAN_MAC :=           LAN_MAC;
      Resp.STATUS_LAN :=        StatusLanToStr(STATUS_LAN);
      Resp.NIVEL_BATERIA :=     NivelBateriaToStr(NIVEL_BATERIA);
      Resp.MT_TOTAL :=          MT_TOTAL;
      Resp.MT_USADA :=          MT_USADA;
      Resp.DH_ATUAL :=          DH_ATUAL;
      Resp.VER_SB :=            VER_SB;
      Resp.VER_LAYOUT :=        VER_LAYOUT;
      Resp.ULTIMO_CFe :=        ULTIMO_CFe;
      Resp.LISTA_INICIAL :=     LISTA_INICIAL;
      Resp.LISTA_FINAL :=       LISTA_FINAL;
      Resp.DH_CFe :=            DH_CFe;
      Resp.DH_ULTIMA :=         DH_ULTIMA;
      Resp.CERT_EMISSAO :=      CERT_EMISSAO;
      Resp.CERT_VENCIMENTO :=   CERT_VENCIMENTO;
      Resp.ESTADO_OPERACAO :=   EstadoOperacaoToStr(ESTADO_OPERACAO);

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;
    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaCriarCFe(ArqCFe: String);
var
  Resp: TRetornoCriarCFe;
begin
  Resp := TRetornoCriarCFe.Create(resINI);
  try
    with fACBrSAT.CFe do
    begin
      Resp.nCFe := IntToStr(ide.nCFe);
      Resp.XML  := AsXMLString;
      Resp.Arquivo:= ArqCFe;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;
    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaEnviarDadosVenda(Resultado: String);
var
  ArqCFe: String;
  Resp: TRetornoEnvio;
begin
  Resp := TRetornoEnvio.Create(resINI);
  try
    with fACBrSAT do
    begin
      ArqCFe := CFe.NomeArquivo;
      Resp.Resultado := Resultado;
      Resp.NumeroSessao  := Resposta.numeroSessao;
      Resp.CodigoDeRetorno := Resposta.codigoDeRetorno;
      Resp.RetornoStr := Resposta.RetornoStr;
      if (ArqCFe <> '') and FileExists(ArqCFe) then
        Resp.Arquivo := ArqCFe;
      Resp.XML := CFe.AsXMLString;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;
    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaCancelarVenda(Resultado: String);
var
  ArqCFe: String;
  Resp: TRetornoCancelarCFe;
begin
  Resp := TRetornoCancelarCFe.Create(resINI);
  try
    with fACBrSAT do
    begin
      ArqCFe := CFeCanc.NomeArquivo;
      Resp.Resultado := Resultado;
      Resp.NumeroSessao  := Resposta.numeroSessao;
      Resp.CodigoDeRetorno := Resposta.codigoDeRetorno;
      Resp.RetornoStr := Resposta.RetornoStr;
      if (ArqCFe <> '') and FileExists(ArqCFe) then
        Resp.Arquivo := ArqCFe;
      Resp.XML := CFeCanc.AsXMLString;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;
    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaTesteFimaFim(Resultado: String);
var
  Resp: TRetornoTesteFimaFim;
begin
  Resp := TRetornoTesteFimaFim.Create(resINI);
  try
    with fACBrSAT do
    begin
      Resp.Resultado := Resultado;
      Resp.NumeroSessao  := Resposta.numeroSessao;
      Resp.CodigoDeRetorno := Resposta.codigoDeRetorno;
      Resp.RetornoStr := Resposta.RetornoStr;
      Resp.XML := CFe.AsXMLString;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;
      RespostaIntegrador;

    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaPadrao;
var
  Resp: TPadraoSATResposta;
begin
  Resp := TPadraoSATResposta.Create('CFe',resINI);
  try
    with fACBrSAT do
    begin
      Resp.Arquivo:= Extrato.NomeDocumento;
      Resp.XML:= Extrato.CFe.XMLOriginal;

      fpCmd.Resposta := sLineBreak + Resp.Gerar;

    end;

  finally
    Resp.Free;
  end;

end;

procedure TACBrObjetoSAT.RespostaIntegrador;
begin
  with fACBrSAT do
    fpCmd.Resposta := fpCmd.Resposta + DoRespostaIntegrador();

end;


end.
