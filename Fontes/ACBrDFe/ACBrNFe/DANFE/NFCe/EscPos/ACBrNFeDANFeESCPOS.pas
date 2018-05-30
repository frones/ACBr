{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 04/04/2013:  André Ferreira de Moraes
|*   Inicio do desenvolvimento
|* 20/11/2014:  Welkson Renny de Medeiros
|*   Contribuições para impressão na Bematech e Daruma
|* 25/11/2014: Régys Silveira
|*   Acertos gerais e adaptação do layout a norma técnica
|*   adição de método para impressão de relatórios
|*   adição de impressão de eventos
|* 28/11/2014: Régys Silveira
|*   Implementação da possibilidade de utilizar tags nos relatorios (segue o
|*   padrão do acbrecf)
|* 06/05/2015: DSA
|*   Refatoração para usar TACBrPosPrinter
******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeDANFeESCPOS;

interface

uses
  Classes, SysUtils, {$IFDEF FPC} LResources, {$ENDIF}
  ACBrNFeDANFEClass, ACBrPosPrinter,
  pcnNFe, pcnEnvEventoNFe;

type
  { TACBrNFeDANFeESCPOS }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrNFeDANFeESCPOS = class(TACBrNFeDANFEClass)
  private
    FPosPrinter : TACBrPosPrinter ;
    procedure AjustaStringList(AStringList: TStringList);
    procedure MontarEnviarDANFE(NFE: TNFe; const AResumido: Boolean);
    procedure SetPosPrinter(AValue: TACBrPosPrinter);
  protected
    FpNFe: TNFe;
    FpEvento: TEventoNFe;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AtivarPosPrinter;

    procedure ConfigurarRegiao(AEsquerda, ATopo, AAltura, ALargura: Integer);

    procedure GerarCabecalho;
    procedure GerarIdentificacaodoDANFE;
    function GerarMensagemContingencia(CaracterDestaque : Char): String;
    procedure GerarDetalhesProdutosServicos;
    procedure GerarInformacoesTotais;
    procedure GerarPagamentos;
    procedure GerarInformacoesConsultaChaveAcesso;
    function GerarInformacoesConsumidor(Lateral: Boolean = False): String;
    function GerarInformacoesIdentificacaoNFCe(Lateral: Boolean = False): String;
    procedure GerarMensagemFiscal;
    procedure GerarInformacoesQRCode(Cancelamento: Boolean = False; Lateral: Boolean = False);
    procedure GerarMensagemInteresseContribuinte;
    procedure GerarTotalTributos;

    procedure GerarRodape;
    procedure GerarDadosEvento;
    procedure GerarObservacoesEvento;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDANFE(NFE: TNFe = nil); override;
    procedure ImprimirDANFEResumido(NFE: TNFe = nil); override;
    procedure ImprimirDANFECancelado(NFE: TNFe = nil); override;
    procedure ImprimirEVENTO(NFE : TNFe = nil);override;

    procedure ImprimirRelatorio(const ATexto: TStrings; const AVias: Integer = 1;
      const ACortaPapel: Boolean = True; const ALogo : Boolean = True);
  published
    property PosPrinter : TACBrPosPrinter read FPosPrinter write SetPosPrinter;
  end;

procedure Register;

implementation

uses
  strutils, Math,
  ACBrNFe, ACBrValidador, ACBrUtil, ACBrDFeUtil, ACBrConsts,
   pcnConversao, pcnAuxiliar;

procedure Register;
begin
  RegisterComponents('ACBrNFe', [TACBrNFeDANFeESCPOS]);
end;

{ TACBrNFeDANFeESCPOS }

constructor TACBrNFeDANFeESCPOS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPosPrinter := Nil;
end;

destructor TACBrNFeDANFeESCPOS.Destroy;
begin
  inherited Destroy;
end;


procedure TACBrNFeDANFeESCPOS.SetPosPrinter(AValue: TACBrPosPrinter);
begin
  if AValue <> FPosPrinter then
  begin
     if Assigned(FPosPrinter) then
        FPosPrinter.RemoveFreeNotification(Self);

     FPosPrinter := AValue;

     if AValue <> nil then
        AValue.FreeNotification(self);
  end ;
end;

procedure TACBrNFeDANFeESCPOS.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent is TACBrPosPrinter) and (FPosPrinter <> nil) then
       FPosPrinter := nil ;
  end;
end;

procedure TACBrNFeDANFeESCPOS.AtivarPosPrinter;
begin
  if not Assigned( FPosPrinter ) then
    raise Exception.Create('Componente PosPrinter não associado');

  FPosPrinter.Ativar;
end;

procedure TACBrNFeDANFeESCPOS.ConfigurarRegiao(AEsquerda, ATopo, AAltura,
  ALargura: Integer);
begin
  FPosPrinter.Buffer.Add('<mp_esquerda>'+IntToStr(AEsquerda)+'</mp_esquerda>'+
                         '<mp_topo>'+IntToStr(ATopo)+'</mp_topo>'+
                         '<mp_altura>'+IntToStr(AAltura)+'</mp_altura>'+
                         '<mp_largura>'+IntToStr(ALargura)+'</mp_largura>'+
                         '<mp_espaco>'+IntToStr(FPosPrinter.EspacoEntreLinhas)+'</mp_espaco>'+
                         '</mp_configurar>');
end;

procedure TACBrNFeDANFeESCPOS.GerarCabecalho;
var
  DadosCabecalho: TStringList;
  Lateral: Boolean;
  Altura: Integer;
  TextoLateral: String;
begin
  Lateral := ImprimeLogoLateral and (PosPrinter.TagsNaoSuportadas.IndexOf(cTagModoPaginaLiga) < 0);
  if Lateral then
  begin
    TextoLateral := '<c><n>';
    if (Trim(FpNFe.Emit.xFant) <> '') and ImprimeNomeFantasia then
       TextoLateral := TextoLateral + FpNFe.Emit.xFant + ' </n>';

    TextoLateral := TextoLateral +
                    QuebraLinhas('<n>' + FpNFe.Emit.xNome + '</n>'+
                    ' CNPJ:'+ FormatarCNPJ(FpNFe.Emit.CNPJCPF) +
                    ' IE:'+FormatarIE(FpNFe.Emit.IE,FpNFe.Emit.EnderEmit.UF),
                    Trunc(FPosPrinter.ColunasFonteCondensada/2))+sLineBreak;

    TextoLateral := TextoLateral +
                    QuebraLinhas(Trim(Trim(FpNFe.Emit.EnderEmit.xLgr) + ', ' +
                    Trim(FpNFe.Emit.EnderEmit.nro) + ' ' +
                    ifthen(Trim(FpNFe.Emit.EnderEmit.xCpl)<>'',Trim(FpNFe.Emit.EnderEmit.xCpl) + ' ','') +
                    ifthen(Trim(FpNFe.Emit.EnderEmit.xBairro)<>'',Trim(FpNFe.Emit.EnderEmit.xBairro) + ' ','') +
                    Trim(FpNFe.Emit.EnderEmit.xMun) + '-' + Trim(FpNFe.Emit.EnderEmit.UF) + ' ' +
                    Trim(FpNFe.Emit.EnderEmit.fone)), Trunc(FPosPrinter.ColunasFonteCondensada/2));

    DadosCabecalho := TStringList.Create;
    try
      DadosCabecalho.Text := TextoLateral;
      Altura := max(43*DadosCabecalho.Count,250);
    finally
      DadosCabecalho.Free;
    end;
    FPosPrinter.Buffer.Add('</zera><mp>');
    ConfigurarRegiao(0,0,Altura,260);
    FPosPrinter.Buffer.Add('</logo>');
    ConfigurarRegiao(260,0,Altura,325);
    FPosPrinter.Buffer.Add(TextoLateral);
    FPosPrinter.Buffer.Add('</mp>');
  end
  else
  begin
    FPosPrinter.Buffer.Add('</zera></ce></logo>');

    if (Trim(FpNFe.Emit.xFant) <> '') and ImprimeNomeFantasia then
       FPosPrinter.Buffer.Add('</ce><c><n>' +  FpNFe.Emit.xFant + '</n>');

    FPosPrinter.Buffer.Add('</ce><c>CNPJ: '+ FormatarCNPJ(FpNFe.Emit.CNPJCPF) + ' <n>' + FpNFe.Emit.xNome + '</n>');

    FPosPrinter.Buffer.Add('<c>' + QuebraLinhas(Trim(FpNFe.Emit.EnderEmit.xLgr) + ', ' +
      Trim(FpNFe.Emit.EnderEmit.nro) + '  ' +
      Trim(FpNFe.Emit.EnderEmit.xCpl) + '  ' +
      Trim(FpNFe.Emit.EnderEmit.xBairro) +  ' ' +
      Trim(FpNFe.Emit.EnderEmit.xMun) + '-' + Trim(FpNFe.Emit.EnderEmit.UF)
      , FPosPrinter.ColunasFonteCondensada)
    );

    if not EstaVazio(FpNFe.Emit.EnderEmit.fone) then
      FPosPrinter.Buffer.Add('</ce></fn><c>Fone: <n>'+ FormatarFone(FpNFe.Emit.EnderEmit.fone)+'</n> I.E.: '+FormatarIE(FpNFe.Emit.IE,FpNFe.Emit.EnderEmit.UF))
    else
      FPosPrinter.Buffer.Add('</ce></fn><c>I.E.: '+FormatarIE(FpNFe.Emit.IE,FpNFe.Emit.EnderEmit.UF))
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarIdentificacaodoDANFE;
begin
  FPosPrinter.Buffer.Add('</ce><c><n>' +
    QuebraLinhas(ACBrStr('Documento Auxiliar da Nota Fiscal de Consumidor Eletrônica'), FPosPrinter.ColunasFonteCondensada) + 
    '</n>');
  FPosPrinter.Buffer.Add(GerarMensagemContingencia('='));
end;

procedure TACBrNFeDANFeESCPOS.GerarDetalhesProdutosServicos;
var
  i: Integer;
  nTamDescricao: Integer;
  VlrAcrescimo, VlrLiquido: Double;
  sItem, sCodigo, sDescricao, sQuantidade, sUnidade, sVlrUnitario, sVlrProduto,
    LinhaCmd: String;
begin
  if ImprimirItens then
  begin
    FPosPrinter.Buffer.Add('</ae><c>'+ACBrStr(PadSpace('#|Código|Descrição|Qtde|Un|Valor unit.|Valor total',
                                            FPosPrinter.ColunasFonteCondensada, '|')));

    for i := 0 to FpNFe.Det.Count - 1 do
    begin
      with FpNFe.Det.Items[i] do
      begin
        sItem        :=        IntToStrZero( Prod.nItem, 3);
	sCodigo      :=        ManterCodigo( Prod.cEAN , Prod.cProd );
        sDescricao   :=                Trim( Prod.xProd);
        sQuantidade  :=    FormatQuantidade( Prod.QCom, False );
        sUnidade     :=                Trim( Prod.uCom);
        sVlrUnitario := FormatValorUnitario( Prod.VUnCom );
        sVlrProduto  :=       FormatFloatBr( Prod.vProd );

        if ImprimeEmUmaLinha then
        begin
          LinhaCmd := sItem + ' ' + sCodigo + ' ' + '[DesProd] ' + sQuantidade + ' ' +
            sUnidade + ' X ' + sVlrUnitario + ' ' + sVlrProduto;

          // acerta tamanho da descrição
          nTamDescricao := FPosPrinter.ColunasFonteCondensada - Length(LinhaCmd) + 9;
          sDescricao := PadRight(Copy(sDescricao, 1, nTamDescricao), nTamDescricao);

          LinhaCmd := StringReplace(LinhaCmd, '[DesProd]', sDescricao, [rfReplaceAll]);
          FPosPrinter.Buffer.Add('</ae><c>' + LinhaCmd);
        end
        else
        begin
          LinhaCmd := sItem + ' ' + sCodigo + ' ' + sDescricao;
          FPosPrinter.Buffer.Add('</ae><c>' + LinhaCmd);

          LinhaCmd :=
            PadRight(sQuantidade, 15) + ' ' + PadRight(sUnidade, 6) + ' X ' +
            PadRight(sVlrUnitario, 13) + '|' + sVlrProduto;
          LinhaCmd := padSpace(LinhaCmd, FPosPrinter.ColunasFonteCondensada, '|');
          FPosPrinter.Buffer.Add('</ae><c>' + LinhaCmd);
        end;

        if ImprimeDescAcrescItem then
        begin
          VlrAcrescimo := Prod.vFrete + Prod.vSeg + Prod.vOutro;
          VlrLiquido   := (Prod.qCom * Prod.vUnCom) + VlrAcrescimo - Prod.vDesc;

          // desconto
          if Prod.vDesc > 0 then
          begin
            LinhaCmd := '</ae><c>' + padSpace(
                'desconto ' + padLeft(FormatFloatBr(Prod.vDesc, '-,0.00'), 15, ' ')
                +IIf((VlrAcrescimo > 0),'','|' + FormatFloatBr(VlrLiquido)) ,
                FPosPrinter.ColunasFonteCondensada, '|');
            FPosPrinter.Buffer.Add('</ae><c>' + LinhaCmd);
          end;

          // acrescimo
          if VlrAcrescimo > 0 then
          begin
            LinhaCmd := '</ae><c>' + ACBrStr(padSpace(
                'acréscimo ' + padLeft(FormatFloatBr(VlrAcrescimo, '+,0.00'), 15, ' ')
                + '|' + FormatFloatBr(VlrLiquido),
                FPosPrinter.ColunasFonteCondensada, '|'));
            FPosPrinter.Buffer.Add('</ae><c>' + LinhaCmd);
          end;
        end;
        //Informação Adicional do produto
        if infAdProd <> '' then
        begin
          LinhaCmd := StringReplace(infAdProd, ';', sLineBreak, [rfReplaceAll]);
          FPosPrinter.Buffer.Add('<c>'+LinhaCmd);
        end;
      end;
    end;
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarInformacoesTotais;
begin
  FPosPrinter.Buffer.Add('<c>' + PadSpace('Qtde. Total de Itens|' +
     IntToStrZero(FpNFe.Det.Count, 3), FPosPrinter.ColunasFonteCondensada, '|'));

  FPosPrinter.Buffer.Add('<c>' + PadSpace('Valor Total R$|' +
     FormatFloatBr(FpNFe.Total.ICMSTot.vProd + FpNFe.Total.ISSQNtot.vServ),
     FPosPrinter.ColunasFonteCondensada, '|'));

  if (FpNFe.Total.ICMSTot.vDesc > 0) then
    FPosPrinter.Buffer.Add('<c>' + PadSpace('Descontos|' +
       FormatFloatBr(FpNFe.Total.ICMSTot.vDesc, '-,0.00'),
       FPosPrinter.ColunasFonteCondensada, '|'));

  if (FpNFe.Total.ICMSTot.vOutro+FpNFe.Total.ICMSTot.vSeg) > 0 then
    FPosPrinter.Buffer.Add('<c>' + ACBrStr(PadSpace('Acréscimos|' +
       FormatFloatBr(FpNFe.Total.ICMSTot.vOutro+FpNFe.Total.ICMSTot.vSeg, '+,0.00'),
       FPosPrinter.ColunasFonteCondensada, '|')));

  if (FpNFe.Total.ICMSTot.vFrete) > 0 then
    FPosPrinter.Buffer.Add('<c>' + ACBrStr(PadSpace('Frete|' +
       FormatFloatBr(FpNFe.Total.ICMSTot.vFrete, '+,0.00'),
       FPosPrinter.ColunasFonteCondensada, '|')));

  if (FpNFe.Total.ICMSTot.vDesc > 0) or
     ((FpNFe.Total.ICMSTot.vOutro+FpNFe.Total.ICMSTot.vFrete+FpNFe.Total.ICMSTot.vSeg) > 0) then
    FPosPrinter.Buffer.Add('</ae><e>' + PadSpace('Valor a Pagar R$|' +
       FormatFloatBr(FpNFe.Total.ICMSTot.vNF),
       FPosPrinter.ColunasFonteCondensada div 2, '|') + '</e>');
end;

procedure TACBrNFeDANFeESCPOS.GerarPagamentos;
var
  i: Integer;
  Troco: Real;
begin
  //Total := 0;
  FPosPrinter.Buffer.Add('<c>' + PadSpace('FORMA DE PAGAMENTO | VALOR PAGO R$',
     FPosPrinter.ColunasFonteCondensada, '|'));

  for i := 0 to FpNFe.pag.Count - 1 do
  begin
    FPosPrinter.Buffer.Add('<c>' + ACBrStr(PadSpace(FormaPagamentoToDescricao(FpNFe.pag.Items[i].tPag) +
       '|' + FormatFloatBr(FpNFe.pag.Items[i].vPag),
       FPosPrinter.ColunasFonteCondensada, '|')));
  end;

  Troco := IIf(FpNFe.pag.vTroco > 0,FpNFe.pag.vTroco,vTroco);

  if Troco > 0 then
    FPosPrinter.Buffer.Add('<c>' + PadSpace('Troco R$|' +
       FormatFloatBr(Troco), FPosPrinter.ColunasFonteCondensada, '|'));

end;

procedure TACBrNFeDANFeESCPOS.GerarInformacoesConsultaChaveAcesso;
begin
  // chave de acesso
  FPosPrinter.Buffer.Add('</ce><c><n>Consulte pela Chave de Acesso em</n>');
  if EstaVazio(FpNFe.infNFeSupl.urlChave) then
    FPosPrinter.Buffer.Add('</ce><c>'+TACBrNFe(ACBrNFe).GetURLConsultaNFCe(FpNFe.ide.cUF, FpNFe.ide.tpAmb, FpNFe.infNFe.Versao))
  else
    FPosPrinter.Buffer.Add('</ce><c>'+FpNFe.infNFeSupl.urlChave);
  FPosPrinter.Buffer.Add('</ce><c>' + FormatarChaveAcesso(OnlyNumber(FpNFe.infNFe.ID)));
end;

procedure TACBrNFeDANFeESCPOS.GerarTotalTributos;
var
  MsgTributos : String;
begin
  if not ImprimirTributos then
    Exit;

  if TributosSeparadamente and ((vTribFed+vTribEst+vTribMun) > 0) then
  begin
     MsgTributos:= 'Tributos Incidentes Lei Federal 12.741/12 - Total R$ %s Federal R$ %s Estadual R$ %s Municipal R$ %s';
     FPosPrinter.Buffer.Add('<c>' + QuebraLinhas(Format(MsgTributos,[FormatFloatBr(vTribFed + vTribEst + vTribMun),
                         FormatFloatBr(vTribFed),
                         FormatFloatBr(vTribEst),
                         FormatFloatBr(vTribMun)]),FPosPrinter.ColunasFonteCondensada));
  end
  else
  begin
    if FpNFe.Total.ICMSTot.vTotTrib > 0 then
    begin
      MsgTributos:= 'Tributos Totais Incidentes(Lei Federal 12.741/12): R$ %s';
      FPosPrinter.Buffer.Add('<c>' + QuebraLinhas(Format(MsgTributos,[FormatFloatBr(FpNFe.Total.ICMSTot.vTotTrib)]),
                          FPosPrinter.ColunasFonteCondensada));
    end;
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarMensagemInteresseContribuinte;
var
  TextoObservacao: AnsiString;
begin
  TextoObservacao := Trim(FpNFe.InfAdic.infCpl);
  if TextoObservacao <> '' then
  begin
    TextoObservacao := StringReplace(FpNFe.InfAdic.infCpl, ';', sLineBreak, [rfReplaceAll]);
    FPosPrinter.Buffer.Add('<c>' + TextoObservacao);
  end;
end;

function TACBrNFeDANFeESCPOS.GerarMensagemContingencia(CaracterDestaque: Char
  ): String;
var
  MsgContingencia: TStringList;
  Lateral: Boolean;
begin
  Lateral := (CaracterDestaque=#0);
  MsgContingencia := TStringList.Create;
  try
    // se homologação imprimir o texto de homologação
    if (FpNFe.ide.tpAmb = taHomologacao) then
      MsgContingencia.Add('</ce><c><n>'+
        QuebraLinhas(ACBrStr('EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL'),
                     Trunc(FPosPrinter.ColunasFonteCondensada/ifthen(Lateral,2,1)))+
        '</n>');

    // se diferente de normal imprimir a emissão em contingência
    if (FpNFe.ide.tpEmis <> teNormal) and
       EstaVazio(FpNFe.procNFe.nProt) then
    begin
      MsgContingencia.Add(ACBrStr('</c></ce><e><n>EMITIDA EM CONTINGÊNCIA</n></e>'));
      if CaracterDestaque <> #0 then
        MsgContingencia.Add(ACBrStr('<c><n>'+PadCenter('Pendente de autorização',FPosPrinter.ColunasFonteCondensada, CaracterDestaque)+'</n>'))
      else
        MsgContingencia.Add(ACBrStr('<c><n>Pendente de autorização</n>'));
    end;
  finally
    Result := MsgContingencia.Text;
    MsgContingencia.Free;
  end;
end;

function TACBrNFeDANFeESCPOS.GerarInformacoesConsumidor(Lateral: Boolean
  ): String;
var
  LinhaCmd: String;
  DadosConsumidor: TStringList;
begin
  DadosConsumidor := TStringList.Create;
  try
    if (FpNFe.Dest.idEstrangeiro = '') and (FpNFe.Dest.CNPJCPF = '') then
    begin
      DadosConsumidor.Add(ACBrStr('<c>CONSUMIDOR NÃO IDENTIFICADO'));
    end
    else
    begin
      if FpNFe.Dest.idEstrangeiro <> '' then
        LinhaCmd := 'CONSUMIDOR - Id. Estrangeiro' +ifthen(Lateral,sLineBreak,' ')+ FpNFe.Dest.idEstrangeiro
      else
      begin
        if Length(Trim(FpNFe.Dest.CNPJCPF)) > 11 then
          LinhaCmd := 'CONSUMIDOR - CNPJ '+ FormatarCNPJ(FpNFe.Dest.CNPJCPF)
        else
          LinhaCmd := 'CONSUMIDOR - CPF ' + FormatarCPF(FpNFe.Dest.CNPJCPF);
      end;

      if Lateral then
      begin
        DadosConsumidor.Add('</ce><c><n>' + QuebraLinhas(LinhaCmd, TruncFix(FPosPrinter.ColunasFonteCondensada/2)) + '</n> ');
        DadosConsumidor.Add(QuebraLinhas(Trim(FpNFe.Dest.xNome), TruncFix(FPosPrinter.ColunasFonteCondensada/2)));
      end
      else
      begin
        LinhaCmd := '</ce><c><n>' + LinhaCmd + '</n> ' + Trim(FpNFe.Dest.xNome);
        DadosConsumidor.Add(QuebraLinhas(LinhaCmd, FPosPrinter.ColunasFonteCondensada));
      end;

      LinhaCmd := Trim(
        Trim(FpNFe.Dest.EnderDest.xLgr) + ' ' +
        IfThen(Trim(FpNFe.Dest.EnderDest.xLgr) = '','',Trim(FpNFe.Dest.EnderDest.nro)) + ' ' +
        Trim(FpNFe.Dest.EnderDest.xCpl) + ' ' +
        Trim(FpNFe.Dest.EnderDest.xBairro) + ' ' +
        Trim(FpNFe.Dest.EnderDest.xMun) + ' ' +
        Trim(FpNFe.Dest.EnderDest.UF)
      );
      if LinhaCmd <> '' then
        DadosConsumidor.Add('<c>' + QuebraLinhas(LinhaCmd,Trunc(FPosPrinter.ColunasFonteCondensada/ifthen(Lateral,2,1))));
    end;
  finally
    Result := DadosConsumidor.Text;
    DadosConsumidor.Free;
  end;
end;

function TACBrNFeDANFeESCPOS.GerarInformacoesIdentificacaoNFCe(Lateral: Boolean
  ): String;
var
  Via : String;
  DadosNFe: TStringList;
begin
  DadosNFe := TStringList.Create;
  try
    if EstaVazio(Trim(FpNFe.procNFe.nProt)) then
      Via := IfThen(ViaConsumidor, '|Via Consumidor', '|Via Empresa')
    else
      Via := '';
    // dados da nota eletronica de consumidor
    DadosNFe.Add('</ce><c><n>' + StringReplace(QuebraLinhas(ACBrStr(
      'NFC-e nº ' + IntToStrZero(FpNFe.Ide.nNF, 9) +
      ' Série ' + IntToStrZero(FpNFe.Ide.serie, 3) +ifthen(Lateral,'|',' ')+
      DateTimeToStr(FpNFe.ide.dEmi) +
      Via+'</n>')
      , Trunc(FPosPrinter.ColunasFonteCondensada/ifthen(Lateral,2,1)), '|'), '|', ' ', [rfReplaceAll]));

    // protocolo de autorização
    if (FpNFe.Ide.tpEmis <> teOffLine) or
       NaoEstaVazio(FpNFe.procNFe.nProt) then
    begin
      DadosNFe.Add(ACBrStr('<c><n>Protocolo de Autorização:</n>')+ifthen(Lateral,sLineBreak,' ')+Trim(FpNFe.procNFe.nProt));
      if (FpNFe.procNFe.dhRecbto <> 0) then
        DadosNFe.Add(ACBrStr('<c><n>Data de Autorização</n>'+ifthen(Lateral,sLineBreak,' ')+DateTimeToStr(FpNFe.procNFe.dhRecbto)+'</fn>'));
    end;
  finally
    Result := DadosNFe.Text;
    DadosNFe.Free;
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarMensagemFiscal;
var
  TextoObservacao: AnsiString;
begin
  TextoObservacao := Trim(FpNFe.InfAdic.infAdFisco);
  if TextoObservacao <> '' then
  begin
    TextoObservacao := StringReplace(FpNFe.InfAdic.infAdFisco, ';', sLineBreak, [rfReplaceAll]);
    FPosPrinter.Buffer.Add('<c>' + TextoObservacao);
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarInformacoesQRCode(Cancelamento: Boolean;
  Lateral: Boolean);
var
  qrcode: AnsiString;
begin
  if Cancelamento then
  begin
    FPosPrinter.Buffer.Add('</fn></linha_simples>');
    FPosPrinter.Buffer.Add('</ce>Consulta via leitor de QR Code');
  end;

  if EstaVazio(Trim(FpNFe.infNFeSupl.qrCode)) then
    qrcode := TACBrNFe(ACBrNFe).GetURLQRCode(
      FpNFe.ide.cUF,
      FpNFe.ide.tpAmb,
      FpNFe.infNFe.ID,
      IfThen(FpNFe.Dest.idEstrangeiro <> '', FpNFe.Dest.idEstrangeiro, FpNFe.Dest.CNPJCPF),
      FpNFe.ide.dEmi,
      FpNFe.Total.ICMSTot.vNF,
      FpNFe.Total.ICMSTot.vICMS,
      FpNFe.signature.DigestValue,
      FpNFe.infNfe.Versao)
  else
    qrcode := FpNFe.infNFeSupl.qrCode;

  if Lateral then
    FPosPrinter.Buffer.Add( '<qrcode_tipo>2</qrcode_tipo>'+
                            '<qrcode_error>0</qrcode_error>'+
                            '<qrcode_largura>4</qrcode_largura>'+
                            '<qrcode>'+qrcode+'</qrcode>')
  else
    FPosPrinter.Buffer.Add( '<qrcode_error>0</qrcode_error>'+
                            '<qrcode>'+qrcode+'</qrcode>');

  if Cancelamento then
  begin
    FPosPrinter.Buffer.Add(ACBrStr('<c>Protocolo de Autorização'));
    FPosPrinter.Buffer.Add('<c>'+Trim(FpNFe.procNFe.nProt) + ' ' +
       IfThen(FpNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FpNFe.procNFe.dhRecbto),
              '') + '</fn>');
    FPosPrinter.Buffer.Add('</linha_simples>');
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarRodape;
begin
  // sistema
  if Sistema <> '' then
    FPosPrinter.Buffer.Add('</ce><c>' + Sistema);

  if Site <> '' then
    FPosPrinter.Buffer.Add('</ce><c>' + Site);

  // pular linhas e cortar o papel
  if FPosPrinter.CortaPapel then
    FPosPrinter.Buffer.Add('</corte_total>')
  else
    FPosPrinter.Buffer.Add('</pular_linhas>')
end;

procedure TACBrNFeDANFeESCPOS.MontarEnviarDANFE(NFE: TNFe;
  const AResumido: Boolean);
var
  Altura: Integer;
  TextoLateral: TStringList;
begin
  if NFE = nil then
  begin
    if not Assigned(ACBrNFe) then
      raise Exception.Create(ACBrStr('Componente ACBrNFe não atribuído'));

    FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFE;
  end
  else
    FpNFe := NFE;

  GerarCabecalho;
  GerarIdentificacaodoDANFE;
  if not AResumido then
    GerarDetalhesProdutosServicos;

  GerarInformacoesTotais;
  GerarPagamentos;
  GerarInformacoesConsultaChaveAcesso;

  if QRCodeLateral and (PosPrinter.Colunas >= 48) and (PosPrinter.TagsNaoSuportadas.IndexOf(cTagModoPaginaLiga) < 0) then
  begin
    TextoLateral := TStringList.Create;
    try
      TextoLateral.Text := sLineBreak +
                           GerarInformacoesConsumidor(True) +
                           GerarInformacoesIdentificacaoNFCe(True) +
                           GerarMensagemContingencia(#0);

      AjustaStringList(TextoLateral); // Ajusta corretamente o numero de Linhas

      Altura := max(43*TextoLateral.Count, 600);
      FPosPrinter.Buffer.Add('<mp>');
      ConfigurarRegiao(0,0,Altura,250);
      GerarInformacoesQRCode(False, True);
      ConfigurarRegiao(270,0,Altura,325);
      FPosPrinter.Buffer.Add(TextoLateral.Text);
      FPosPrinter.Buffer.Add('</mp>');
    finally
      TextoLateral.Free;
    end;
  end
  else
  begin
    FPosPrinter.Buffer.Add(GerarInformacoesConsumidor(False));
    FPosPrinter.Buffer.Add(GerarInformacoesIdentificacaoNFCe);
    GerarMensagemFiscal;
    FPosPrinter.Buffer.Add(GerarMensagemContingencia(#32));
    GerarInformacoesQRCode;
  end;

  GerarMensagemInteresseContribuinte;
  GerarTotalTributos;
  GerarRodape;

  FPosPrinter.Imprimir('',False,True,True,NumCopias);
end;

procedure TACBrNFeDANFeESCPOS.AjustaStringList(AStringList: TStringList);
var
  Linhas: String;
begin
  Linhas := AStringList.Text;
  AStringList.Clear;
  AStringList.Text := Linhas;
end;

procedure TACBrNFeDANFeESCPOS.ImprimirDANFE(NFE: TNFe);
begin
  AtivarPosPrinter;
  MontarEnviarDANFE(NFE, False);
end;

procedure TACBrNFeDANFeESCPOS.ImprimirDANFEResumido(NFE: TNFe);
var
  OldImprimirItens: Boolean;
begin
  AtivarPosPrinter;
  OldImprimirItens := ImprimirItens;
  try
    ImprimirItens := False;
    MontarEnviarDANFE(NFE, True);
  finally
    ImprimirItens := OldImprimirItens;
  end;
end;

procedure TACBrNFeDANFeESCPOS.GerarDadosEvento;
const
  TAMCOLDESCR = 11;
begin
  // dados da nota eletrônica
  FPosPrinter.Buffer.Add('</fn></ce><n>Nota Fiscal para Consumidor Final</n>');
  FPosPrinter.Buffer.Add(ACBrStr('Número: ' + IntToStrZero(FpNFe.ide.nNF, 9) +
                                 ' Série: ' + IntToStrZero(FpNFe.ide.serie, 3)));
  FPosPrinter.Buffer.Add(ACBrStr('Emissão: ' + DateTimeToStr(FpNFe.ide.dEmi)) + '</n>');

  if FpNFe.Total.ICMSTot.vNF > 0 then
    FPosPrinter.Buffer.Add(ACBrStr('Valor Documento: R$ ' +  FormatFloatBr(FpNFe.Total.ICMSTot.vNF)) + '</n>');

  FPosPrinter.Buffer.Add(' ');
  FPosPrinter.Buffer.Add('<c>CHAVE ACESSO');
  FPosPrinter.Buffer.Add(FormatarChaveAcesso(OnlyNumber(FpNFe.infNFe.ID)));
  FPosPrinter.Buffer.Add('</linha_simples>');

  // dados do evento
  FPosPrinter.Buffer.Add('</fn><n>EVENTO</n>');
  FPosPrinter.Buffer.Add('</fn></ae>' + PadRight('Evento:', TAMCOLDESCR) +
     FpEvento.Evento[0].InfEvento.TipoEvento );
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Descrição:', TAMCOLDESCR)) +
     FpEvento.Evento[0].InfEvento.DescEvento);
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Orgão:', TAMCOLDESCR)) +
     IntToStr(FpEvento.Evento[0].InfEvento.cOrgao) );
  FPosPrinter.Buffer.Add( ACBrStr( PadRight('Ambiente:', TAMCOLDESCR) +
     IfThen(FpEvento.Evento[0].RetInfEvento.tpAmb = taProducao,
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


procedure TACBrNFeDANFeESCPOS.GerarObservacoesEvento;
begin
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

procedure TACBrNFeDANFeESCPOS.ImprimirDANFECancelado(NFE: TNFe);
begin
  if NFE = nil then
  begin
    if not Assigned(ACBrNFe) then
      raise Exception.Create(ACBrStr('Componente ACBrNFe não atribuído'));

    if TACBrNFe(ACBrNFe).NotasFiscais.Count <= 0 then
      raise Exception.Create(ACBrStr('XML da NFe não informado, obrigatório para o modelo ESCPOS'))
    else
      FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFE;
  end
  else
    FpNFe := NFE;

  FpEvento := TACBrNFe(ACBrNFe).EventoNFe;
  if not Assigned(FpEvento) then
    raise Exception.Create('Arquivo de Evento não informado!');

  AtivarPosPrinter;
  GerarCabecalho;
  GerarDadosEvento;
  FPosPrinter.Buffer.Add(GerarInformacoesConsumidor);
  GerarObservacoesEvento;
  GerarInformacoesQRCode(True);
  GerarRodape;

  FPosPrinter.Imprimir('',False,True,True,NumCopias);
end;

procedure TACBrNFeDANFeESCPOS.ImprimirEVENTO(NFE: TNFe);
begin
  ImprimirDANFECancelado(NFE);
end;

procedure TACBrNFeDANFeESCPOS.ImprimirRelatorio(const ATexto: TStrings; const AVias: Integer = 1;
      const ACortaPapel: Boolean = True; const ALogo : Boolean = True);
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
{$I ACBrNFeDANFeESCPOS.lrs}
{$ENDIF}

end.
