{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrBPeDABPeESCPOS;

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC} LResources, {$ENDIF}
  ACBrBPeDABPEClass, ACBrPosPrinter, ACBrBase,
  pcnBPe, pcnEnvEventoBPe;

const
  CLarguraRegiaoEsquerda = 270;

type
  { TACBrBPeDABPeESCPOS }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrBPeDABPeESCPOS = class(TACBrBPeDABPEClass)
  private
    FPosPrinter: TACBrPosPrinter;

    procedure MontarEnviarDABPE(BPE: TBPe; const AResumido: Boolean);
    procedure SetPosPrinter(AValue: TACBrPosPrinter);
  protected
    FpBPe: TBPe;
    FpEvento: TEventoBPe;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AtivarPosPrinter;
    procedure GerarMensagemContingencia(CaracterDestaque: Char);

    procedure GerarCabecalhoAgencia;
    procedure GerarCabecalhoEmitente;
    procedure GerarIdentificacaodoDABPE;
    procedure GerarInformacoesViagem;
    procedure GerarInformacoesTotais;
    procedure GerarPagamentos;
    procedure GerarInformacoesConsultaChaveAcesso;
    procedure GerarInformacoesPassageiro;
    procedure GerarInformacoesIdentificacaoBPe;
    procedure GerarInformacoesBoardingPassBarCode;
    procedure GerarInformacoesQRCode(Cancelamento: Boolean = False);
    procedure GerarTotalTributos;
    procedure GerarMensagemFiscal;
    procedure GerarMensagemInteresseContribuinte;
    procedure GerarRodape;

    procedure GerarDadosEvento;
    procedure GerarObservacoesEvento;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDABPE(BPE: TBPe = nil); override;
    procedure ImprimirDABPECancelado(BPe: TBPe = nil); override;
    procedure ImprimirEVENTO(BPe: TBPe = nil);override;

    procedure ImprimirRelatorio(const ATexto: TStrings; const AVias: Integer = 1;
      const ACortaPapel: Boolean = True; const ALogo: Boolean = True);
  published
    property PosPrinter: TACBrPosPrinter read FPosPrinter write SetPosPrinter;
  end;

implementation

uses
  strutils, Math,
  ACBrBPe, ACBrValidador,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrDFeUtil, ACBrConsts,
  pcnConversao, pcnConversaoBPe, pcnAuxiliar;

{ TACBrBPeDABPeESCPOS }

constructor TACBrBPeDABPeESCPOS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPosPrinter := Nil;
end;

destructor TACBrBPeDABPeESCPOS.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrBPeDABPeESCPOS.SetPosPrinter(AValue: TACBrPosPrinter);
begin
  if AValue <> FPosPrinter then
  begin
    if Assigned(FPosPrinter) then
      FPosPrinter.RemoveFreeNotification(Self);

    FPosPrinter := AValue;

    if AValue <> nil then
      AValue.FreeNotification(self);
  end;
end;

procedure TACBrBPeDABPeESCPOS.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent is TACBrPosPrinter) and (FPosPrinter <> nil) then
      FPosPrinter := nil;
  end;
end;

procedure TACBrBPeDABPeESCPOS.AtivarPosPrinter;
begin
  if not Assigned( FPosPrinter ) then
    raise Exception.Create('Componente PosPrinter não associado');

  FPosPrinter.Ativar;
end;

procedure TACBrBPeDABPeESCPOS.GerarMensagemContingencia(CaracterDestaque: Char);
begin
  // se homologação imprimir o texto de homologação
  if (FpBPe.ide.tpAmb = taHomologacao) then
    FPosPrinter.Buffer.Add(ACBrStr('</ce><c><n>EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL</n>'));

  // se diferente de normal imprimir a emissão em contingência
  if (FpBPe.ide.tpEmis <> teNormal) and EstaVazio(FpBPe.procBPe.nProt) then
  begin
    FPosPrinter.Buffer.Add(ACBrStr('</c></ce><e><n>EMITIDA EM CONTINGÊNCIA</n></e>'));
    FPosPrinter.Buffer.Add(ACBrStr('<c><n>' + PadCenter('Pendente de autorização',
                                               FPosPrinter.ColunasFonteCondensada,
                                               CaracterDestaque) + '</n>'));
  end;
end;

procedure TACBrBPeDABPeESCPOS.GerarCabecalhoAgencia;
begin
  if trim(FpBPe.agencia.xNome) <> '' then
  begin
    FPosPrinter.Buffer.Add('</ce><c>' + FormatarCNPJ(FpBPe.agencia.CNPJ) + ' <n>' + FpBPe.agencia.xNome + '</n>');

    FPosPrinter.Buffer.Add('<c>' + QuebraLinhas(Trim(FpBPe.agencia.EnderAgencia.xLgr) + ', ' +
      Trim(FpBPe.agencia.EnderAgencia.nro) + '  ' +
      Trim(FpBPe.agencia.EnderAgencia.xCpl) + '  ' +
      Trim(FpBPe.agencia.EnderAgencia.xBairro) + ' ' +
      Trim(FpBPe.agencia.EnderAgencia.xMun) + '-' + Trim(FpBPe.agencia.EnderAgencia.UF)
      , FPosPrinter.ColunasFonteCondensada)
    );

    FPosPrinter.Buffer.Add('</linha_simples>');
  end;
end;

procedure TACBrBPeDABPeESCPOS.GerarCabecalhoEmitente;
var
  DadosCabecalho: TStringList;
  Lateral: Boolean;
  Altura: Integer;
  TextoLateral: String;
begin
  Lateral := ImprimeLogoLateral and
             (PosPrinter.TagsNaoSuportadas.IndexOf(cTagModoPaginaLiga) < 0);

  if Lateral then
  begin
    TextoLateral := '<c>';
    if (Trim(FpBPe.Emit.xFant) <> '') and ImprimeNomeFantasia then
       TextoLateral := TextoLateral +
                       QuebraLinhas('<n>' + FpBPe.Emit.xFant + ' </n>',
                        Trunc(FPosPrinter.ColunasFonteCondensada/2));

    TextoLateral := TextoLateral +
                    QuebraLinhas('<n>' + FpBPe.Emit.xNome + '</n>'+
                    ' CNPJ:' + FormatarCNPJ(FpBPe.Emit.CNPJ) +
                    ' IE:' + FormatarIE(FpBPe.Emit.IE, FpBPe.Emit.EnderEmit.UF),
                      Trunc(FPosPrinter.ColunasFonteCondensada/2)) + sLineBreak;

    TextoLateral := TextoLateral +
                    QuebraLinhas(Trim(Trim(FpBPe.Emit.EnderEmit.xLgr) +
                    ifthen(Trim(FpBPe.Emit.EnderEmit.nro) <> '', ', ' + Trim(FpBPe.Emit.EnderEmit.nro), '') + ' ' +
                    ifthen(Trim(FpBPe.Emit.EnderEmit.xCpl) <> '', Trim(FpBPe.Emit.EnderEmit.xCpl) + ' ', '') +
                    ifthen(Trim(FpBPe.Emit.EnderEmit.xBairro) <> '', Trim(FpBPe.Emit.EnderEmit.xBairro) + ' ', '') +
                    Trim(FpBPe.Emit.EnderEmit.xMun) + '-' + Trim(FpBPe.Emit.EnderEmit.UF) + ' ' +
                    ifthen(Trim(FpBPe.Emit.EnderEmit.fone) <> '', '<n>' + FormatarFone(Trim(FpBPe.Emit.EnderEmit.fone)) + '</n>', '')),
                      Trunc(FPosPrinter.ColunasFonteCondensada/2));

    DadosCabecalho := TStringList.Create;
    try
      DadosCabecalho.Text := TextoLateral;
      Altura := max(FPosPrinter.CalcularAlturaTexto(DadosCabecalho.Count), 250);
    finally
      DadosCabecalho.Free;
    end;
    FPosPrinter.Buffer.Add('</zera><mp>' +
                           FPosPrinter.ConfigurarRegiaoModoPagina(0, 0, Altura, CLarguraRegiaoEsquerda) +
                           '</logo>');
    FPosPrinter.Buffer.Add(FPosPrinter.ConfigurarRegiaoModoPagina(CLarguraRegiaoEsquerda, 0, Altura, 325) +
                           TextoLateral +
                           '</mp>');
  end
  else
  begin
    FPosPrinter.Buffer.Add('</zera></ce></logo>');

    if (Trim(FpBPe.Emit.xFant) <> '') and ImprimeNomeFantasia then
      FPosPrinter.Buffer.Add('</ce><c><n>' +  FpBPe.Emit.xFant + '</n>');

    FPosPrinter.Buffer.Add('</ce><c>'+ FpBPe.Emit.xNome);
    FPosPrinter.Buffer.Add('</ce><c>'+ FormatarCNPJ(FpBPe.Emit.CNPJ) + ' I.E.: ' +
                           FormatarIE(FpBPe.Emit.IE, FpBPe.Emit.EnderEmit.UF));


    FPosPrinter.Buffer.Add('<c>' +
      QuebraLinhas(Trim(FpBPe.Emit.EnderEmit.xLgr) + ', ' +
        Trim(FpBPe.Emit.EnderEmit.nro) + '  ' +
        Trim(FpBPe.Emit.EnderEmit.xCpl) + '  ' +
        Trim(FpBPe.Emit.EnderEmit.xBairro) +  ' ' +
        Trim(FpBPe.Emit.EnderEmit.xMun) + '-' + Trim(FpBPe.Emit.EnderEmit.UF),
      FPosPrinter.ColunasFonteCondensada));

    if not EstaVazio(FpBPe.Emit.EnderEmit.fone) then
      FPosPrinter.Buffer.Add('</ce></fn><c>Fone: <n>' +
                             FormatarFone(FpBPe.Emit.EnderEmit.fone) + '</n>');
  end;
end;

procedure TACBrBPeDABPeESCPOS.GerarIdentificacaodoDABPe;
begin
  FPosPrinter.Buffer.Add(' ');

  FPosPrinter.Buffer.Add('</ce><c><n>' +
    QuebraLinhas(ACBrStr('Documento Auxiliar do Bilhete de Passagem Eletrônico'), FPosPrinter.ColunasFonteCondensada) +
    '</n>');
  GerarMensagemContingencia('=');

  FPosPrinter.Buffer.Add('</linha_simples>');
end;

procedure TACBrBPeDABPeESCPOS.GerarInformacoesViagem;
var
  i: Integer;
begin
  for i := 0 to FpBPe.infViagem.Count -1 do
  begin
    FPosPrinter.Buffer.Add('</ae><c>' +
                           PadSpace('Origem : <n>' +
                                    FpBPe.infPassagem.xLocOrig + ' (' +
                                    FpBPe.Ide.UFIni + ')</n>', 64, ''));
    FPosPrinter.Buffer.Add('</ae><c>' +
                           PadSpace('Destino: <n>' +
                                    FpBPe.infPassagem.xLocDest + ' (' +
                                    FpBPe.Ide.UFFim + ')</n>', 64, ''));

    if i <> 0 then
      FPosPrinter.Buffer.Add('</ce><c>-- CONEXÃO --');

    FPosPrinter.Buffer.Add(' ');
    {
    FPosPrinter.Buffer.Add('</fn><c>' +
                           PadSpace('Data: <n>' +
                           DateToStr(FpBPe.infPassagem.dhEmb) + '</n>', 32, '') +
                           PadSpace('Horário: <n>' +
                           TimeToStr(FpBPe.infPassagem.dhEmb) + '</n>', 32, ''));
    }
    FPosPrinter.Buffer.Add('</fn>' +
                           PadSpace('Data: <n>' +
                           DateToStr(FpBPe.infPassagem.dhEmb) + '|Horário: <n>' +
                           TimeToStr(FpBPe.infPassagem.dhEmb) + '</n>',
                           FPosPrinter.ColunasFonteNormal, '|'));

    FPosPrinter.Buffer.Add(' ');
    FPosPrinter.Buffer.Add('</fn>' +
                           PadSpace('Poltrona: <n>' +
                           ifthen(FpBPe.infViagem.Items[i].Poltrona > 0, IntToStr(FpBPe.infViagem.Items[i].Poltrona), ' ') +
                           '|Plataforma: <n>' +
                           FpBPe.infViagem.Items[i].Plataforma + '</n>',
                           FPosPrinter.ColunasFonteNormal, '|'));

//    FPosPrinter.Buffer.Add('</ce><c>Poltrona: ' + IntToStr(FpBPe.infViagem.Items[i].Poltrona) +
//                           ' Plataforma: ' + FpBPe.infViagem.Items[i].Plataforma);

    FPosPrinter.Buffer.Add(' ');
    FPosPrinter.Buffer.Add('</ce><c>' +
                           PadSpace('Prefixo:', 32, '') + PadSpace('Tipo:', 32, ''));
    FPosPrinter.Buffer.Add('</ce><c><n>' +
                           PadSpace(FpBPe.infViagem.Items[i].Prefixo, 32, '') +
                           PadSpace(tpServicoToDesc(FpBPe.infViagem.Items[i].tpServ), 32, '') +
                           '</n>');

    FPosPrinter.Buffer.Add('</ce><c>' + PadSpace('Linha:', 64, ''));
    FPosPrinter.Buffer.Add('</ce><c><n>' +
                           PadSpace(FpBPe.infViagem.Items[i].xPercurso, 64, '') +
                           '</n>');
  end;

  FPosPrinter.Buffer.Add('</linha_simples>');
end;

procedure TACBrBPeDABPeESCPOS.GerarInformacoesTotais;
var
  i: Integer;
  Total: Double;
begin
  Total := 0.0;
  for i := 0 to FpBPe.infValorBPe.Comp.Count -1 do
  begin
    FPosPrinter.Buffer.Add('</fn>' + PadSpace(tpComponenteToDesc(FpBpe.infValorBPe.Comp.Items[i].tpComp) + '|' +
     FormatFloatBr(FpBpe.infValorBPe.Comp.Items[i].vComp), FPosPrinter.ColunasFonteNormal, '|'));
    Total := Total + FpBpe.infValorBPe.Comp.Items[i].vComp;
  end;

  FPosPrinter.Buffer.Add('</fn>' + PadSpace('Valor Total R$|' +
     FormatFloatBr(Total), FPosPrinter.ColunasFonteNormal, '|'));

  if FpBPe.infValorBPe.vDesconto > 0 then
    FPosPrinter.Buffer.Add('</fn>' + PadSpace('Desconto R$|' +
       FormatFloatBr(FpBPe.infValorBPe.vDesconto), FPosPrinter.ColunasFonteNormal, '|'));

  FPosPrinter.Buffer.Add('</fn>' + PadSpace('Valor a Pagar R$|' +
     FormatFloatBr(Total - FpBPe.infValorBPe.vDesconto), FPosPrinter.ColunasFonteNormal, '|'));
end;

procedure TACBrBPeDABPeESCPOS.GerarPagamentos;
var
  i: Integer;
begin
  FPosPrinter.Buffer.Add(' ');
  FPosPrinter.Buffer.Add('</fn>' + PadSpace('FORMA DE PAGAMENTO | VALOR PAGO R$',
     FPosPrinter.ColunasFonteNormal, '|'));

  for i := 0 to FpBPe.pag.Count - 1 do
  begin
    FPosPrinter.Buffer.Add('</fn>' + ACBrStr(PadSpace(FormaPagamentoBPeToDescricao(FpBPe.pag.Items[i].tPag) +
       '|' + FormatFloatBr(FpBPe.pag.Items[i].vPag),
       FPosPrinter.ColunasFonteNormal, '|')));
  end;

  if FpBPe.infValorBPe.vTroco > 0 then
    FPosPrinter.Buffer.Add('</fn>' + PadSpace('Troco R$|' +
       FormatFloatBr(FpBPe.infValorBPe.vTroco), FPosPrinter.ColunasFonteNormal, '|'));
end;

procedure TACBrBPeDABPeESCPOS.GerarInformacoesConsultaChaveAcesso;
begin
  // chave de acesso
  FPosPrinter.Buffer.Add(' ');
  FPosPrinter.Buffer.Add('</ce><c><n>Consulte pela Chave de Acesso em</n>');
  FPosPrinter.Buffer.Add('</ce><c>'+TACBrBPe(ACBrBPe).GetURLConsultaBPe(FpBPe.ide.cUF, FpBPe.ide.tpAmb));
  FPosPrinter.Buffer.Add('</ce><c>' + FormatarChaveAcesso(OnlyNumber(FpBPe.infBPe.ID)));
end;

procedure TACBrBPeDABPeESCPOS.GerarInformacoesPassageiro;
var
  LinhaCmd: String;
begin
  FPosPrinter.Buffer.Add(' ');

  if (FpBPe.infPassagem.infPassageiro.xNome = '') then
  begin
    FPosPrinter.Buffer.Add(ACBrStr('<c>PASSAGEIRO NÃO IDENTIFICADO'));
  end
  else
  begin
    LinhaCmd := 'PASSAGEIRO - ' + tpDocumentoToDesc(FpBPe.infPassagem.infPassageiro.tpDoc) +
                ' ' + FpBPe.infPassagem.infPassageiro.nDoc +
                ' - ' + FpBPe.infPassagem.infPassageiro.xNome;

    LinhaCmd := '</ce><c><n>' + LinhaCmd + '</n> ' + Trim(FpBPe.Comp.xNome);
    FPosPrinter.Buffer.Add(QuebraLinhas(LinhaCmd, FPosPrinter.ColunasFonteCondensada));

    if FpBPe.infValorBPe.tpDesconto <> tdNenhum then
      FPosPrinter.Buffer.Add('</ce><c>TIPO DE DESCONTO: ' + tpDescontoToDesc(FpBPe.infValorBPe.tpDesconto));
  end;
end;

procedure TACBrBPeDABPeESCPOS.GerarInformacoesIdentificacaoBPe;
var
  Via: String;
begin
  FPosPrinter.Buffer.Add(' ');

  if EstaVazio(Trim(FpBPe.procBPe.nProt)) then
    Via := IfThen(ViaConsumidor, '|Via Passageiro', '|Via Empresa')
  else
    Via := '';

  // dados da nota eletronica de consumidor
  FPosPrinter.Buffer.Add('</ce><c><n>' + StringReplace(QuebraLinhas(ACBrStr(
    'BP-e nº ' + IntToStrZero(FpBPe.Ide.nBP, 9) +
    ' Série ' + IntToStrZero(FpBPe.Ide.serie, 3) +
    ' ' + DateTimeToStr(FpBPe.ide.dhEmi) +
    Via+'</n>')
    , FPosPrinter.ColunasFonteCondensada, '|'), '|', ' ', [rfReplaceAll]));

  FPosPrinter.Buffer.Add(' ');
  // protocolo de autorização
  if (FpBPe.Ide.tpEmis <> teOffLine) or
     NaoEstaVazio(FpBPe.procBPe.nProt) then
  begin
    FPosPrinter.Buffer.Add(ACBrStr('<c><n>Protocolo de Autorização:</n> ')+Trim(FpBPe.procBPe.nProt));

    if (FpBPe.procBPe.dhRecbto <> 0) then
      FPosPrinter.Buffer.Add(ACBrStr('<c><n>Data de Autorização</n> '+DateTimeToStr(FpBPe.procBPe.dhRecbto)+'</fn>'));
  end;

  GerarMensagemContingencia('=');
end;

procedure TACBrBPeDABPeESCPOS.GerarInformacoesBoardingPassBarCode;
begin
  // Não implementado
end;

procedure TACBrBPeDABPeESCPOS.GerarInformacoesQRCode(Cancelamento: Boolean = False);
var
  qrcode: String;
  ConfigQRCodeErrorLevel: Integer;
begin
  if Cancelamento then
  begin
    FPosPrinter.Buffer.Add('</fn></linha_simples>');
    FPosPrinter.Buffer.Add('</ce>Consulta via leitor de QR Code');
  end;

  if EstaVazio(Trim(FpBPe.infBPeSupl.qrCodBPe)) then
    qrcode := TACBrBPe(ACBrBPe).GetURLQRCode(
      FpBPe.ide.cUF,
      FpBPe.ide.tpAmb,
      FpBPe.infBPe.ID)
  else
    qrcode := FpBPe.infBPeSupl.qrCodBPe;

  ConfigQRCodeErrorLevel := FPosPrinter.ConfigQRCode.ErrorLevel;

  // impressão do qrcode
  FPosPrinter.Buffer.Add( '<qrcode_error>0</qrcode_error>'+
                          '<qrcode>'+qrcode+'</qrcode>'+
                          '<qrcode_error>'+IntToStr(ConfigQRCodeErrorLevel)+'</qrcode_error>');


  if Cancelamento then
  begin
    FPosPrinter.Buffer.Add(ACBrStr('<c>Protocolo de Autorização'));
    FPosPrinter.Buffer.Add('<c>'+Trim(FpBPe.procBPe.nProt) + ' ' +
       IfThen(FpBPe.procBPe.dhRecbto <> 0, DateTimeToStr(FpBPe.procBPe.dhRecbto),
              '') + '</fn>');
    FPosPrinter.Buffer.Add('</linha_simples>');
  end;
end;

procedure TACBrBPeDABPeESCPOS.GerarTotalTributos;
var
  MsgTributos: String;
begin
  if FpBPe.Imp.vTotTrib > 0 then
  begin
    MsgTributos:= 'Tributos Totais Incidentes(Lei Federal 12.741/12): R$ %s';
    FPosPrinter.Buffer.Add('<c>' + QuebraLinhas(Format(MsgTributos,[FormatFloatBr(FpBPe.Imp.vTotTrib)]),
                         FPosPrinter.ColunasFonteCondensada));
  end;
end;

procedure TACBrBPeDABPeESCPOS.GerarMensagemFiscal;
var
  TextoObservacao: String;
begin
  TextoObservacao := Trim(FpBPe.InfAdic.infAdFisco);
  if TextoObservacao <> '' then
  begin
    TextoObservacao := StringReplace(FpBPe.InfAdic.infAdFisco, ';', sLineBreak, [rfReplaceAll]);
    FPosPrinter.Buffer.Add('<c>' + TextoObservacao);
  end;
end;

procedure TACBrBPeDABPeESCPOS.GerarMensagemInteresseContribuinte;
var
  TextoObservacao: String;
begin
  TextoObservacao := Trim(FpBPe.InfAdic.infCpl);
  if TextoObservacao <> '' then
  begin
    TextoObservacao := StringReplace(FpBPe.InfAdic.infCpl, ';', sLineBreak, [rfReplaceAll]);
    FPosPrinter.Buffer.Add('<c>' + TextoObservacao);
  end;
end;

procedure TACBrBPeDABPeESCPOS.GerarRodape;
begin
  // sistema
  if Sistema <> '' then
    FPosPrinter.Buffer.Add('</ce><c>' + Sistema);

  if Site <> '' then
    FPosPrinter.Buffer.Add('</ce><c>' + Site);

  FPosPrinter.Buffer.Add('</pular_linhas>');

  // pular linhas e cortar o papel
  if FPosPrinter.CortaPapel then
    FPosPrinter.Buffer.Add('</corte_total>')
  else
    FPosPrinter.Buffer.Add('</pular_linhas>');
end;

procedure TACBrBPeDABPeESCPOS.MontarEnviarDABPe(BPe: TBPe;
  const AResumido: Boolean);
begin
  if BPe = nil then
  begin
    if not Assigned(ACBrBPe) then
      raise Exception.Create(ACBrStr('Componente ACBrBPe não atribuído'));

    FpBPe := TACBrBPe(ACBrBPe).Bilhetes.Items[0].BPe;
  end
  else
    FpBPe := BPe;

  GerarCabecalhoAgencia;
  GerarCabecalhoEmitente;
  GerarIdentificacaodoDABPE;
  GerarInformacoesViagem;
  GerarInformacoesTotais;
  GerarPagamentos;
  GerarInformacoesConsultaChaveAcesso;
  GerarInformacoesPassageiro;
  GerarInformacoesIdentificacaoBPe;
  GerarInformacoesBoardingPassBarCode;
  GerarInformacoesQRCode;
  GerarTotalTributos;
  GerarMensagemFiscal;
  GerarMensagemInteresseContribuinte;
  GerarRodape;

  FPosPrinter.Imprimir('',False,True,True,NumCopias);
end;

procedure TACBrBPeDABPeESCPOS.ImprimirDABPe(BPe: TBPe);
begin
  AtivarPosPrinter;
  MontarEnviarDABPe(BPe, False);
end;

procedure TACBrBPeDABPeESCPOS.GerarDadosEvento;
const
  TAMCOLDESCR = 11;
begin
  // dados da nota eletrônica
  FPosPrinter.Buffer.Add('</fn></ce><n>Bilhete de Passagem Eletrônico</n>');
  FPosPrinter.Buffer.Add(ACBrStr('Número: ' + IntToStrZero(FpBPe.ide.nBP, 9) +
                                 ' Série: ' + IntToStrZero(FpBPe.ide.serie, 3)));
  FPosPrinter.Buffer.Add(ACBrStr('Emissão: ' + DateTimeToStr(FpBPe.ide.dhEmi)) + '</n>');
  FPosPrinter.Buffer.Add(' ');
  FPosPrinter.Buffer.Add('<c>CHAVE ACESSO');
  FPosPrinter.Buffer.Add(FormatarChaveAcesso(OnlyNumber(FpBPe.infBPe.ID)));
  FPosPrinter.Buffer.Add('</linha_simples>');

  // dados do evento
  FPosPrinter.Buffer.Add('</fn><n>EVENTO</n>');
  FPosPrinter.Buffer.Add('</fn></ae>' + PadRight('Evento:', TAMCOLDESCR) +
     FpEvento.Evento[0].Infevento.TipoEvento );
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Descrição:', TAMCOLDESCR)) +
     FpEvento.Evento[0].Infevento.DescEvento);
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Orgão:', TAMCOLDESCR)) +
     IntToStr(FpEvento.Evento[0].Infevento.cOrgao) );
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Ambiente:', TAMCOLDESCR) +
     IfThen(FpEvento.Evento[0].RetInfevento.tpAmb = taProducao,
            'PRODUCAO', 'HOMOLOGAÇÃO - SEM VALOR FISCAL') ));
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Emissão:', TAMCOLDESCR)) +
     DateTimeToStr(FpEvento.Evento[0].InfEvento.dhEvento) );
  FPosPrinter.Buffer.Add( PadRight('Sequencia:', TAMCOLDESCR) +
     IntToStr(FpEvento.Evento[0].InfEvento.nSeqEvento) );
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Versão:', TAMCOLDESCR)) +
     FpEvento.Evento[0].InfEvento.versaoEvento );
  FPosPrinter.Buffer.Add( PadRight('Status:', TAMCOLDESCR) +
     FpEvento.Evento[0].RetInfEvento.xMotivo );
  FPosPrinter.Buffer.Add( PadRight('Protocolo:', TAMCOLDESCR) +
     FpEvento.Evento[0].RetInfEvento.nProt );
  FPosPrinter.Buffer.Add( PadRight('Registro:', TAMCOLDESCR) +
     DateTimeToStr(FpEvento.Evento[0].RetInfEvento.dhRegEvento) );

  FPosPrinter.Buffer.Add('</linha_simples>');
end;

procedure TACBrBPeDABPeESCPOS.GerarObservacoesEvento;
const
  TAMCOLDESCR = 25;
begin
  if FpEvento.Evento[0].InfEvento.detEvento.vTotBag > 0 then
  begin
    FPosPrinter.Buffer.Add('</linha_simples>');
    FPosPrinter.Buffer.Add( PadRight('Quatidade de Bagagem..:', TAMCOLDESCR) +
     IntToStr(FpEvento.Evento[0].InfEvento.detEvento.qBagagem) );
    FPosPrinter.Buffer.Add( PadRight('Valor Total da Bagagem:', TAMCOLDESCR) +
     FormatFloatBr(FpEvento.Evento[0].InfEvento.detEvento.vTotBag) );
  end;

  if FpEvento.Evento[0].InfEvento.detEvento.xJust <> '' then
  begin
    FPosPrinter.Buffer.Add('</linha_simples>');
    FPosPrinter.Buffer.Add('</fn></ce><n>JUSTIFICATIVA</n>');
    FPosPrinter.Buffer.Add('</fn></ae>' +
       FpEvento.Evento[0].InfEvento.detEvento.xJust );
  end

  else if FpEvento.Evento[0].InfEvento.detEvento.xCorrecao <> '' then
  begin
    FPosPrinter.Buffer.Add('</linha_simples>');
    FPosPrinter.Buffer.Add('</fn></ce><n>' + ACBrStr('CORREÇÃO') + '</n>' );
    FPosPrinter.Buffer.Add('</fn></ae>' +
       FpEvento.Evento[0].InfEvento.detEvento.xCorrecao );
  end;
end;

procedure TACBrBPeDABPeESCPOS.ImprimirDABPECancelado(BPe: TBPe);
begin
  if BPe = nil then
  begin
    if not Assigned(ACBrBPe) then
      raise Exception.Create(ACBrStr('Componente ACBrBPe não atribuído'));

    if TACBrBPe(ACBrBPe).Bilhetes.Count <= 0 then
      raise Exception.Create(ACBrStr('XML do BPe não informado, obrigatório para o modelo ESCPOS'))
    else
      FpBPe := TACBrBPe(ACBrBPe).Bilhetes.Items[0].BPe;
  end
  else
    FpBPe := BPe;

  FpEvento := TACBrBPe(ACBrBPe).EventoBPe;
  if not Assigned(FpEvento) then
    raise Exception.Create('Arquivo de Evento não informado!');

  AtivarPosPrinter;
  GerarCabecalhoEmitente;
  GerarDadosEvento;
  GerarInformacoesPassageiro;
  GerarObservacoesEvento;
  GerarInformacoesQRCode(True);
  GerarRodape;

  FPosPrinter.Imprimir;
end;

procedure TACBrBPeDABPeESCPOS.ImprimirEVENTO(BPe: TBPe);
begin
  ImprimirDABPeCancelado(BPe);
end;

procedure TACBrBPeDABPeESCPOS.ImprimirRelatorio(const ATexto: TStrings; const AVias: Integer = 1;
      const ACortaPapel: Boolean = True; const ALogo: Boolean = True);
var
  LinhaCmd: String;
begin
  LinhaCmd := '</zera>';

  if ALogo then
    LinhaCmd := LinhaCmd + '</ce></logo>';

  LinhaCmd := LinhaCmd + '</ae>';
  FPosPrinter.Buffer.Add(LinhaCmd);

  FPosPrinter.Buffer.AddStrings( ATexto );

  if ACortaPapel then
    FPosPrinter.Buffer.Add('</corte_parcial>')
  else
    FPosPrinter.Buffer.Add('</pular_linhas>');

  FPosPrinter.Imprimir('', True, True, True, AVias);
end;

{$IFDEF FPC}

initialization
{$I ACBrBPeDABPeESCPOS.lrs}
{$ENDIF}

end.
