{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit pnfsEnvLoteRpsResposta;

interface

uses
  SysUtils, Classes, Variants, Math, StrUtils, Contnrs,
  ACBrUtil,
  pcnAuxiliar, pcnConversao, pcnLeitor, pnfsConversao, pnfsNFSe;

type

  TMsgRetornoEnvCollection     = class;
  TMsgRetornoEnvCollectionItem = class;
  TInfRec = class;

 TChaveNFeRPSCollectionItem = class(TObject)
  private
    FChaveNFeRPS: TChaveNFeRPS;
  public
    constructor Create;
    destructor Destroy; override;

    property ChaveNFeRPS: TChaveNFeRPS read FChaveNFeRPS write FChaveNFeRPS;
  end;

 TChaveNFeRPSCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TChaveNFeRPSCollectionItem;
    procedure SetItem(Index: Integer; Value: TChaveNFeRPSCollectionItem);
  public
    function Add: TChaveNFeRPSCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TChaveNFeRPSCollectionItem;
    property Items[Index: Integer]: TChaveNFeRPSCollectionItem read GetItem write SetItem; default;
  end;

  TInfRec = class
  private
    FNumeroLote: String;
    FDataRecebimento: TDateTime;
    FProtocolo: String;
    FSucesso: String;
    FMsgRetorno: TMsgRetornoEnvCollection;
    FInformacoesLote: TInformacoesLote;
    FListaChaveNFeRPS: TChaveNFeRPSCollection;

    procedure SetMsgRetorno(Value: TMsgRetornoEnvCollection);
    procedure SetListaChaveNFeRPS(const Value: TChaveNFeRPSCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property NumeroLote: String                   read FNumeroLote      write FNumeroLote;
    property DataRecebimento: TDateTime           read FDataRecebimento write FDataRecebimento;
    property Protocolo: String                    read FProtocolo       write FProtocolo;
    property Sucesso: String                      read FSucesso         write FSucesso;
    property MsgRetorno: TMsgRetornoEnvCollection read FMsgRetorno      write SetMsgRetorno;
    property InformacoesLote: TInformacoesLote    read FInformacoesLote write FInformacoesLote;
    property ListaChaveNFeRPS: TChaveNFeRPSCollection  read FListaChaveNFeRPS     write SetListaChaveNFeRPS;
  end;

 TMsgRetornoEnvCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TMsgRetornoEnvCollectionItem;
    procedure SetItem(Index: Integer; Value: TMsgRetornoEnvCollectionItem);
  public
    function Add: TMsgRetornoEnvCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TMsgRetornoEnvCollectionItem;
    property Items[Index: Integer]: TMsgRetornoEnvCollectionItem read GetItem write SetItem; default;
  end;

 TMsgRetornoEnvCollectionItem = class(TObject)
  private
    FCodigo: String;
    FMensagem: String;
    FCorrecao: String;
    FIdentificacaoRps: TMsgRetornoIdentificacaoRps;
    FChaveNFeRPS: TChaveNFeRPS;
  public
    constructor Create;
    destructor Destroy; override;

    property Codigo: String   read FCodigo   write FCodigo;
    property Mensagem: String read FMensagem write FMensagem;
    property Correcao: String read FCorrecao write FCorrecao;
    property IdentificacaoRps: TMsgRetornoIdentificacaoRps  read FIdentificacaoRps write FIdentificacaoRps;
    property ChaveNFeRPS: TChaveNFeRPS read FChaveNFeRPS write FChaveNFeRPS;
  end;

  TretEnvLote = class(TObject)
  private
    FLeitor: TLeitor;
    FInfRec: TInfRec;
    FProvedor: TnfseProvedor;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    function LerXml_ABRASF: Boolean;

    function LerXml_proCONAM: Boolean;
    function LerXml_proEquiplano: Boolean;
    function LerXml_proEL: Boolean;
    function LerXml_proGoverna: Boolean;
    function LerXml_proInfisc: Boolean;
    function LerXml_proISSDSF: Boolean;
    function LerXml_proNFSeBrasil: Boolean;
    function LerXml_proSP: Boolean;
    function LerXML_proFriburgo: Boolean;
    function LerXml_proCTA: Boolean;
    function LerXML_proSmarapd: Boolean;
    function LerXML_proIPM: Boolean;
    function LerXML_proGiap: Boolean;
    function LerXML_proAssessorPublica: Boolean;

    property Leitor: TLeitor         read FLeitor   write FLeitor;
    property InfRec: TInfRec         read FInfRec   write FInfRec;
    property Provedor: TnfseProvedor read FProvedor write FProvedor;
  end;

implementation

{ TInfRec }

constructor TInfRec.Create;
begin
  inherited Create;
  FMsgRetorno       := TMsgRetornoEnvCollection.Create;
  FInformacoesLote  := TInformacoesLote.Create;
  FListaChaveNFeRPS := TChaveNFeRPSCollection.Create;
end;

destructor TInfRec.Destroy;
begin
  FMsgRetorno.Free;
  FInformacoesLote.Free;
  FListaChaveNFeRPS.Free;
  inherited;
end;

procedure TInfRec.SetListaChaveNFeRPS(const Value: TChaveNFeRPSCollection);
begin
  FListaChaveNFeRPS := Value;
end;

procedure TInfRec.SetMsgRetorno(Value: TMsgRetornoEnvCollection);
begin
  FMsgRetorno.Assign(Value);
end;

{ TMsgRetornoEnvCollection }

function TMsgRetornoEnvCollection.Add: TMsgRetornoEnvCollectionItem;
begin
  Result := Self.New;
end;

function TMsgRetornoEnvCollection.GetItem(
  Index: Integer): TMsgRetornoEnvCollectionItem;
begin
  Result := TMsgRetornoEnvCollectionItem(inherited GetItem(Index));
end;

procedure TMsgRetornoEnvCollection.SetItem(Index: Integer;
  Value: TMsgRetornoEnvCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TMsgRetornoEnvCollection.New: TMsgRetornoEnvCollectionItem;
begin
  Result := TMsgRetornoEnvCollectionItem.Create;
  Self.Add(Result);
end;

{ TMsgRetornoEnvCollectionItem }

constructor TMsgRetornoEnvCollectionItem.Create;
begin
  inherited Create;
  FIdentificacaoRps      := TMsgRetornoIdentificacaoRps.Create;
  FIdentificacaoRps.Tipo := trRPS;
  FChaveNFeRPS           := TChaveNFeRPS.Create;
end;

destructor TMsgRetornoEnvCollectionItem.Destroy;
begin
  FIdentificacaoRps.Free;
  FChaveNFeRPS.Free;
  inherited;
end;

{ TChaveNFeRPSCollection }

function TChaveNFeRPSCollection.Add: TChaveNFeRPSCollectionItem;
begin
  Result := Self.New;
end;

function TChaveNFeRPSCollection.GetItem(
  Index: Integer): TChaveNFeRPSCollectionItem;
begin
  Result := TChaveNFeRPSCollectionItem(inherited GetItem(Index));
end;

procedure TChaveNFeRPSCollection.SetItem(Index: Integer;
  Value: TChaveNFeRPSCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TChaveNFeRPSCollection.New: TChaveNFeRPSCollectionItem;
begin
  Result := TChaveNFeRPSCollectionItem.Create;
  Self.Add(Result);
end;

{ TChaveNFeRPSCollectionItem }

constructor TChaveNFeRPSCollectionItem.Create;
begin
  inherited Create;
  FChaveNFeRPS := TChaveNFeRPS.Create;
end;

destructor TChaveNFeRPSCollectionItem.Destroy;
begin
  FChaveNFeRPS.Free;
  inherited;
end;

{ TretEnvLote }

constructor TretEnvLote.Create;
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FInfRec := TInfRec.Create
end;

destructor TretEnvLote.Destroy;
begin
  FLeitor.Free;
  FinfRec.Free;
  inherited;
end;

function TretEnvLote.LerXml: Boolean;
var
  i: Integer;
begin
  if Provedor = proISSCuritiba then
    Leitor.Arquivo := RemoverNameSpace(Leitor.Arquivo)
  else
    Leitor.Arquivo := RemoverNameSpace(RemoverAtributos(RetirarPrefixos(Leitor.Arquivo, Provedor), Provedor));

  Leitor.Grupo := Leitor.Arquivo;

  case Provedor of
    proCONAM:      Result := LerXml_proCONAM;
    proEL:         Result := LerXml_proEL;
    proEquiplano:  Result := LerXml_proEquiplano;
    proGoverna:    Result := LerXml_proGoverna;
    proInfisc,
    proInfiscv11:  Result := LerXml_proInfisc;
    proISSDSF:     Result := LerXml_proISSDSF;
    proNFSeBrasil: Result := LerXml_proNFSeBrasil;
    proSP,
    proNotaBlu:    Result := LerXml_proSP;
    proFriburgo:   Result := LerXML_proFriburgo;
    proCTA:        Result := LerXml_proCTA;
    proSMARAPD:    Result := LerXML_proSmarapd;
    proIPM:        Result := LerXML_proIPM;
    proGiap:       Result := LerXML_proGiap;
    proAssessorPublico : Result := LerXML_proAssessorPublica;
  else
    Result := LerXml_ABRASF;
  end;

  i := 0;
  while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
  begin
    InfRec.FMsgRetorno.New;
    InfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
    if InfRec.FMsgRetorno[i].FCodigo = '' then
      InfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Code');

    InfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
    if InfRec.FMsgRetorno[i].FMensagem = '' then
      InfRec.FMsgRetorno[i].FMensagem   := Leitor.rCampo(tcStr, 'Reason');

    InfRec.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Detail');

    inc(i);
  end;
end;

function TretEnvLote.LerXml_ABRASF: Boolean;
var
  i: Integer;
  iNivel: Integer;
  Ok: Boolean;
begin
  try
    Result := True;

    infRec.FNumeroLote := Leitor.rCampo(tcStr, 'NumeroLote');
    infRec.FProtocolo  := Leitor.rCampo(tcStr, 'Protocolo');

    // Alguns provedores retornam apenas a data, sem o horário
    if Length(Leitor.rCampo(tcStr, 'DataRecebimento')) > 10
     then infRec.FDataRecebimento := Leitor.rCampo(tcDatHor, 'DataRecebimento')
     else infRec.FDataRecebimento := Leitor.rCampo(tcDat, 'DataRecebimento');

    iNivel := 1;
    if (leitor.rExtrai(2, 'ListaMensagemRetorno') <> '') or
       (leitor.rExtrai(2, 'ListaMensagemRetornoLote') <> '') then
      iNivel := 3
    else
      if (leitor.rExtrai(1, 'ListaMensagemRetorno') <> '') or
         (leitor.rExtrai(1, 'ListaMensagemRetornoLote') <> '') then
        iNivel := 2;

    i := 0;
    while Leitor.rExtrai(iNivel, 'MensagemRetorno', '', i + 1) <> '' do
    begin
      InfRec.FMsgRetorno.New;
      InfRec.FMsgRetorno[i].FIdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
      InfRec.FMsgRetorno[i].FIdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
      InfRec.FMsgRetorno[i].FIdentificacaoRps.Tipo   := StrToTipoRPS(Ok, Leitor.rCampo(tcStr, 'Tipo'));

      InfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
      InfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
      InfRec.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

      inc(i);
    end;

    if Provedor = proISSDigital then
    begin
      i := 0;
      while Leitor.rExtrai(1, 'ListaMensagemRetorno', '', i + 1) <> '' do
      begin
        InfRec.FMsgRetorno.New;
        InfRec.FMsgRetorno[i].FIdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
        InfRec.FMsgRetorno[i].FIdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
        InfRec.FMsgRetorno[i].FIdentificacaoRps.Tipo   := StrToTipoRPS(Ok, Leitor.rCampo(tcStr, 'Tipo'));

        InfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        InfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
        InfRec.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

        inc(i);
      end;
    end;

    i := 0;
    while Leitor.rExtrai(iNivel, 'ErroWebServiceResposta', '', i + 1) <> '' do
    begin
      InfRec.FMsgRetorno.New;
      InfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'CodigoErro');
      InfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'MensagemErro');
      InfRec.FMsgRetorno[i].FCorrecao := '';

      inc(i);
    end;
    {
    i := 0;
    while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
    begin
      InfRec.FMsgRetorno.New;
      InfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
      if InfRec.FMsgRetorno[i].FCodigo = '' then
        InfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Code');

      InfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
      if InfRec.FMsgRetorno[i].FMensagem = '' then
        InfRec.FMsgRetorno[i].FMensagem   := Leitor.rCampo(tcStr, 'Reason');

      InfRec.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Detail');

      inc(i);
    end;
    }
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXML_proAssessorPublica: Boolean;
var
  erro, msg : string;
begin
  Result := True;
  try
    msg := Trim(Leitor.rCampo(tcStr,'Mensagem'));
    erro := Trim(Leitor.rExtrai(1,'ERRO'));
    if erro <> '' then begin
      FInfRec.FMsgRetorno.New;
      FInfRec.FMsgRetorno[0].FCodigo   := '';
      FInfRec.FMsgRetorno[0].FMensagem := Leitor.rCampo(tcStr, 'ERRO');
      FInfRec.FMsgRetorno[0].FCorrecao := '';
    end
    else begin
      if StrToIntDef(msg,-1) > 0 then begin
        FInfRec.Protocolo := msg;
        FInfRec.Sucesso := 'Sim';
        FInfRec.DataRecebimento := Now;
      end;
    end;

  except
    Result := False;
  end;

  Result := Result and (erro = '');

end;

function TretEnvLote.LerXml_proISSDSF: Boolean;
var
  i, posI, count: Integer;
  strAux: AnsiString;
  leitorAux: TLeitor;
begin
  try
    Result := True;

    if leitor.rExtrai(1, 'RetornoEnvioLoteRPS') <> '' then
    begin
      if (leitor.rExtrai(2, 'Cabecalho') <> '') then
      begin
        FInfRec.FSucesso := Leitor.rCampo(tcStr, 'Sucesso');
        if (FInfRec.FSucesso = 'true') then
        begin
          FInfRec.FNumeroLote      := Leitor.rCampo(tcStr, 'NumeroLote');
          FInfRec.FProtocolo       := Leitor.rCampo(tcStr, 'NumeroLote');
          FinfRec.FDataRecebimento := Leitor.rCampo(tcDatHor, 'DataEnvioLote')
        end;
      end;

      i := 0;
      if (leitor.rExtrai(1, 'Alertas') <> '') then
      begin
        strAux := leitor.rExtrai(1, 'Alertas');
        if (strAux <> '') then
        begin
          posI := pos('<Alerta>', strAux);

          while ( posI > 0 ) do
          begin
            count := pos('</Alerta>', strAux) + 7;

            FInfRec.FMsgRetorno.New;

            LeitorAux := TLeitor.Create;
            leitorAux.Arquivo := copy(strAux, PosI, count);
            leitorAux.Grupo   := leitorAux.Arquivo;

            FInfRec.FMsgRetorno[i].FCodigo   := leitorAux.rCampo(tcStr, 'Codigo');
            FInfRec.FMsgRetorno[i].FMensagem := leitorAux.rCampo(tcStr, 'Descricao');
            FInfRec.FMsgRetorno[i].FCorrecao := '';

            inc(i);
            LeitorAux.free;

            Delete(strAux, PosI, count);
            posI := pos('<Alerta>', strAux);
          end;
        end;
      end;

      if (leitor.rExtrai(1, 'Erros') <> '') then
      begin
        strAux := leitor.rExtrai(1, 'Erros');
        if (strAux <> '') then
        begin
            //i := 0 ;
          posI := pos('<Erro>', strAux);

          while (posI > 0) do
          begin
            count := pos('</Erro>', strAux) + 6;

            FInfRec.FMsgRetorno.New;

            LeitorAux := TLeitor.Create;
            leitorAux.Arquivo := copy(strAux, PosI, count);
            leitorAux.Grupo   := leitorAux.Arquivo;

            FInfRec.FMsgRetorno[i].FCodigo   := leitorAux.rCampo(tcStr, 'Codigo');
            FInfRec.FMsgRetorno[i].FMensagem := leitorAux.rCampo(tcStr, 'Descricao');
            FInfRec.FMsgRetorno[i].FCorrecao := '';

            inc(i);
            LeitorAux.free;

            Delete(strAux, PosI, count);
            posI := pos('<Erro>', strAux);
          end;
        end;
      end;
    end
    else
    begin
      if leitor.rExtrai(1, 'enviarResponse') <> '' then
      begin
        if leitor.rExtrai(2, 'enviarReturn') <> '' then
        begin
          i := 0;
          FInfRec.FMsgRetorno.New;
          FInfRec.FMsgRetorno[i].FCodigo   := '';
          FInfRec.FMsgRetorno[i].FMensagem := Leitor.Grupo;
          FInfRec.FMsgRetorno[i].FCorrecao := '';
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXml_proCTA: Boolean;
var
  i, posI, count: Integer;
  strAux: AnsiString;
  leitorAux: TLeitor;
begin
  try
    Result := True;

    Leitor.Arquivo := RetirarPrefixos(Leitor.Arquivo, Provedor);
    Leitor.Grupo   := Leitor.Arquivo;
    if (leitor.rExtrai(1, 'RetornoEnvioLoteRPS') <> '')
      or (leitor.rExtrai(1, 'ReqEnvioLoteRPS') <> '')
    then
    begin
      if (leitor.rExtrai(2, 'Cabecalho') <> '') then
      begin
        FInfRec.FSucesso := Leitor.rCampo(tcStr, 'Sucesso');
        if (FInfRec.FSucesso = 'true') then
        begin
          FInfRec.FNumeroLote      := Leitor.rCampo(tcStr, 'NumeroLote');
          FInfRec.FProtocolo       := Leitor.rCampo(tcStr, 'NumeroLote');
          FinfRec.FDataRecebimento := Leitor.rCampo(tcDatHor, 'DataEnvioLote')
        end;
      end;

      i := 0;
      while Leitor.rExtrai(2, 'ChaveNFSeRPS', '', i + 1) <> '' do
      begin
        InfRec.FListaChaveNFeRPS.New;
        InfRec.ListaChaveNFeRPS[i].ChaveNFeRPS.Numero := Leitor.rCampo(tcStr, 'NumeroNFe');
        InfRec.ListaChaveNFeRPS[i].ChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
        InfRec.ListaChaveNFeRPS[i].ChaveNFeRPS.NumeroRPS := Leitor.rCampo(tcStr, 'NumeroRPS');
        Inc(i);
      end;

      i := 0;
      if (leitor.rExtrai(1, 'Alertas') <> '') then
      begin
        strAux := leitor.rExtrai(1, 'Alertas');
        if (strAux <> '') then
        begin
          posI := pos('<Alerta>', strAux);

          while ( posI > 0 ) do
          begin
            count := pos('</Alerta>', strAux) + 7;

            FInfRec.FMsgRetorno.New;

            LeitorAux := TLeitor.Create;
            leitorAux.Arquivo := copy(strAux, PosI, count);
            leitorAux.Grupo   := leitorAux.Arquivo;

            FInfRec.FMsgRetorno[i].FCodigo   := leitorAux.rCampo(tcStr, 'Codigo');
            FInfRec.FMsgRetorno[i].FMensagem := leitorAux.rCampo(tcStr, 'Descricao');
            FInfRec.FMsgRetorno[i].FCorrecao := '';

            inc(i);
            LeitorAux.free;

            Delete(strAux, PosI, count);
            posI := pos('<Alerta>', strAux);
          end;
        end;
      end;

      if (leitor.rExtrai(1, 'Erros') <> '') then
      begin
        strAux := leitor.rExtrai(1, 'Erros');
        if (strAux <> '') then
        begin
            //i := 0 ;
          posI := pos('<Erro>', strAux);

          while (posI > 0) do
          begin
            count := pos('</Erro>', strAux) + 6;

            FInfRec.FMsgRetorno.New;

            LeitorAux := TLeitor.Create;
            leitorAux.Arquivo := copy(strAux, PosI, count);
            leitorAux.Grupo   := leitorAux.Arquivo;

            FInfRec.FMsgRetorno[i].FCodigo   := leitorAux.rCampo(tcStr, 'Codigo');
            if leitorAux.rCampo(tcStr, 'Descricao') <> '' then
              FInfRec.FMsgRetorno[i].FMensagem := leitorAux.rCampo(tcStr, 'Descricao')
            else
              FInfRec.FMsgRetorno[i].FMensagem := leitorAux.rCampo(tcStr, 'Erro');
            FInfRec.FMsgRetorno[i].FCorrecao := '';

            inc(i);
            LeitorAux.free;

            Delete(strAux, PosI, count);
            posI := pos('<Erro>', strAux);
          end;
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXml_proEquiplano: Boolean;
var
  i: Integer;
begin
  try
    Result := True;

    infRec.FNumeroLote      := Leitor.rCampo(tcStr, 'nrLote');
    infRec.FDataRecebimento := Leitor.rCampo(tcDatHor, 'dtRecebimento');
    infRec.FProtocolo       := Leitor.rCampo(tcStr, 'nrProtocolo');

    if leitor.rExtrai(1, 'mensagemRetorno') <> '' then
    begin
      i := 0;
      if (leitor.rExtrai(2, 'listaErros') <> '') then
      begin
        while Leitor.rExtrai(3, 'erro', '', i + 1) <> '' do
        begin
          InfRec.FMsgRetorno.New;
          InfRec.FMsgRetorno[i].FCodigo  := Leitor.rCampo(tcStr, 'cdMensagem');
          InfRec.FMsgRetorno[i].FMensagem:= Leitor.rCampo(tcStr, 'dsMensagem');
          InfRec.FMsgRetorno[i].FCorrecao:= Leitor.rCampo(tcStr, 'dsCorrecao');

          inc(i);
        end;
      end;

      if (leitor.rExtrai(2, 'listaAlertas') <> '') then
      begin
        while Leitor.rExtrai(3, 'alerta', '', i + 1) <> '' do
        begin
          InfRec.FMsgRetorno.New;
          InfRec.FMsgRetorno[i].FCodigo  := Leitor.rCampo(tcStr, 'cdMensagem');
          InfRec.FMsgRetorno[i].FMensagem:= Leitor.rCampo(tcStr, 'dsMensagem');
          InfRec.FMsgRetorno[i].FCorrecao:= Leitor.rCampo(tcStr, 'dsCorrecao');

          inc(i);
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXML_proFriburgo: Boolean;
var
  i : Integer;
begin
  try
    Result := True;

    FInfRec.Protocolo       := Leitor.rCampo(tcStr, 'Protocolo');
    FInfRec.NumeroLote      := Leitor.rCampo(tcStr, 'NumeroLote');
    FInfRec.DataRecebimento := Leitor.rCampo(tcDatHor, 'DataRecebimento');

    i := 0;
    while Leitor.rExtrai(1, 'Erro', '', i + 1) <> '' do
    begin
      FInfRec.MsgRetorno.New;
      FInfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'ErroID');
      FInfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'ErroMensagem');
      FInfRec.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'ErroSolucao');;

      Inc(i);
    end;

    if FInfRec.MsgRetorno.Count > 0 then
    begin
      FInfRec.Protocolo := '';
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXml_proInfisc: Boolean;
var
  sMotCod, sMotDes: String;
begin
  try
    Result := True;

    if leitor.rExtrai(1, 'confirmaLote') <> '' then
    begin
      FInfRec.FSucesso := Leitor.rCampo(tcStr, 'sit');
      if (FInfRec.FSucesso = '100') then // 100-Aceito
      begin
        FInfRec.FNumeroLote      := Leitor.rCampo(tcStr, 'cLote');
        FInfRec.FProtocolo       := Leitor.rCampo(tcStr, 'cLote');
        FinfRec.FDataRecebimento := Leitor.rCampo(tcDatHor, 'dhRecbto')
      end
      else if (FInfRec.FSucesso = '200') then // 200-Rejeitado
      begin
        sMotDes:=Leitor.rCampo(tcStr, 'mot');
        if Pos('Error', sMotDes) > 0 then
          sMotCod := OnlyNumber(copy(sMotDes, 1, Pos(' ', sMotDes)))
        else
          sMotCod := '';
        InfRec.MsgRetorno.New;
        InfRec.MsgRetorno[0].FCodigo   := sMotCod;
        InfRec.MsgRetorno[0].FMensagem := sMotDes + ' ' +
                                          'CNPJ ' + Leitor.rCampo(tcStr, 'CNPJ') + ' ' +
                                          'DATA ' + Leitor.rCampo(tcStr, 'dhRecbto');
        InfRec.MsgRetorno[0].FCorrecao := '';
      end;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXml_proEL: Boolean;
var
  i: Integer;
  Cod, Msg: String;
  strAux: AnsiString;
begin
  try
    Result := True;

    infRec.FNumeroLote      := Leitor.rCampo(tcStr, 'numeroLote');
    infRec.FDataRecebimento := Leitor.rCampo(tcDatHor, 'dataRecebimento');
    infRec.FProtocolo       := Leitor.rCampo(tcStr, 'numeroProtocolo');

    if (Leitor.rExtrai(1, 'mensagens') <> '') then
    begin
      i := 0;
      while Leitor.rExtrai(1, 'mensagens', '', i + 1) <> '' do
      begin
        strAux := Leitor.rCampo(tcStr, 'mensagens');
        Cod    := Copy(strAux, 1, 4);
        Msg    := Copy(strAux, 8, Length(strAux));
        if Trim(Msg) <> '' then
        begin
          InfRec.FMsgRetorno.New;
          InfRec.FMsgRetorno[i].FCodigo   := Cod;
          InfRec.FMsgRetorno[i].FMensagem := Msg;
          InfRec.FMsgRetorno[i].FCorrecao := '';
          Inc(i);
        end
        else
          Break;
      end;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXml_proNFSeBrasil: Boolean;
var
  i, posI, count: Integer;
  VersaoXML: String;
  strAux, strAux2: AnsiString;
  leitorAux: TLeitor;
begin
  try
    Result := True;

    VersaoXML := '1';
    strAux := leitor.rExtrai(1, 'RespostaLoteRps');

    if ( strAux <> emptystr) then
    begin
      FInfRec.FSucesso := Leitor.rCampo(tcStr, 'Sucesso');

      if (FInfRec.FSucesso <> emptystr) then
      begin
        FInfRec.FNumeroLote :=  Leitor.rCampo(tcStr, 'NumeroLote');
        FInfRec.Protocolo   :=  Leitor.rCampo(tcStr, 'Protocolo');
      end;

      infRec.FProtocolo := Leitor.rCampo(tcStr, 'Protocolo');

      if AnsiUpperCase(infRec.FProtocolo) = 'NAO FOI GERADO NUMERO DE PROTOCOLO PARA ESSA TRANSACAO.' then
        infRec.FProtocolo := '';
    end;

    i := 0;
    strAux := leitor.rExtrai(1, 'erros');

    if ( strAux <> emptystr) then
    begin
      posI := 1;
      while ( posI > 0 ) do
      begin
        count := pos('</erro>', strAux) + 7;

        LeitorAux := TLeitor.Create;
        leitorAux.Arquivo := copy(strAux, PosI, count);
        leitorAux.Grupo   := leitorAux.Arquivo;
//        strAux2 := leitorAux.rExtrai(1,'erro');
        strAux2 := Leitor.rCampo(tcStr, 'erro');
        FInfRec.FMsgRetorno.New;
        FInfRec.FMsgRetorno.Items[i].Mensagem := Leitor.rCampo(tcStr, 'erro')+#13;
        inc(i);
        LeitorAux.free;
        Delete(strAux, PosI, count);
        posI := pos('<erro>', strAux);
      end;
    end;

    strAux := leitor.rExtrai(1, 'confirmacoes');

    if ( strAux <> emptystr) then
    begin
      posI := 1;
      while ( posI > 0 ) do
      begin
        count := pos('</confirmacao>', strAux) + 7;
        LeitorAux := TLeitor.Create;
        leitorAux.Arquivo := copy(strAux, PosI, count);
        leitorAux.Grupo   := leitorAux.Arquivo;
//        strAux2 := leitorAux.rExtrai(1,'confirmacao');
        strAux2 := Leitor.rCampo(tcStr, 'confirmacao');
        FInfRec.FMsgRetorno.New;
        FInfRec.FMsgRetorno.Items[i].Mensagem := Leitor.rCampo(tcStr, 'confirmacao')+#13;
        inc(i);
        LeitorAux.free;
        Delete(strAux, PosI, count);
        posI := pos('<confirmacao>', strAux);
      end;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXml_proGoverna: Boolean;
var
  i, j, MsgErro: Integer;
  Msg: String;
begin
  try
    Result := True;

    if (Leitor.rExtrai(1, 'RetornoLoteRps') <> '') then
    begin
      j := 0;
      i := 0;
      MsgErro := 0;

      while Leitor.rExtrai(1, 'RetornoLoteRps', '', i + 1) <> '' do
      begin
        while Leitor.rExtrai(2, 'DesOco', '', j + 1) <> '' do
        begin
          Msg  := Leitor.rCampo(tcStr, 'DesOco');
          if (Pos('OK!', Msg) = 0) and (Pos('importado com sucesso', Msg) = 0) then
          begin
            InfRec.FMsgRetorno.New;
            InfRec.FMsgRetorno[MsgErro].FMensagem := Msg;
            Inc(MsgErro);
          end;
          inc(j);
        end;

        j := 0;

        while Leitor.rExtrai(2, 'InfRetRps', '', j + 1) <> '' do
        begin
          if (Leitor.rCampo(tcStr,'FlgRet') = 'V') then //V = Verdadeiro | F = Falso
          begin
            InfRec.DataRecebimento := Now;
            InfRec.FProtocolo := '0';
            InfRec.Sucesso := Leitor.rCampo(tcStr,'FlgRet');
            InfRec.FListaChaveNFeRPS.New;
            InfRec.ListaChaveNFeRPS[j].ChaveNFeRPS.Numero := Leitor.rCampo(tcStr, 'NumNot');
            InfRec.ListaChaveNFeRPS[j].ChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodVer');
            InfRec.ListaChaveNFeRPS[j].ChaveNFeRPS.NumeroRPS := Leitor.rCampo(tcStr, 'NumRPS');
          end;
          Inc(j);
        end;

        inc(i);
      end;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXml_proCONAM: boolean;
var
  sMotCod,sMotDes: string;
  i: integer;
begin
  try
    Result := True;

    if leitor.rExtrai(1, 'Sdt_processarpsout') <> '' then begin
        FInfRec.FSucesso := Leitor.rCampo(tcStr, 'Id');
        if (FInfRec.FSucesso = 'Arquivo Aceito') then  begin
            FInfRec.FNumeroLote      := Leitor.rCampo(tcStr, 'Protocolo');
            FInfRec.FProtocolo       := Leitor.rCampo(tcStr, 'Protocolo');
        end;
        if leitor.rExtrai(2, 'Messages') <> '' then begin
            i := 0;
            while Leitor.rExtrai(3, 'Message', '', i + 1) <> '' do begin
                sMotCod:=Leitor.rCampo(tcStr, 'Id');
                sMotDes:=Leitor.rCampo(tcStr, 'Description');

                InfRec.MsgRetorno.New;
                InfRec.MsgRetorno[i].FCodigo   := sMotCod;
                InfRec.MsgRetorno[i].FMensagem := sMotDes;
                Inc(i);
            end;
        end;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXml_proSP: Boolean;
var
  i: Integer;
begin
  try
    Result := True;

    if leitor.rExtrai(1, 'RetornoEnvioLoteRPS') <> '' then
    begin
      if (leitor.rExtrai(2, 'Cabecalho') <> '') then
      begin
        FInfRec.FSucesso := Leitor.rCampo(tcStr, 'Sucesso');
        if (leitor.rExtrai(3, 'InformacoesLote') <> '') then
        begin
          FInfRec.Protocolo                           := Leitor.rCampo(tcStr, 'NumeroLote');
          if FInfRec.Protocolo = '0' then
            FInfRec.Protocolo := '';
            
          FInfRec.NumeroLote                          := Leitor.rCampo(tcStr, 'NumeroLote');
          FInfRec.DataRecebimento                     := Leitor.rCampo(tcDatHor, 'DataEnvioLote');

          FInfRec.InformacoesLote.NumeroLote          := Leitor.rCampo(tcStr, 'NumeroLote');
          FInfRec.InformacoesLote.InscricaoPrestador  := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfRec.InformacoesLote.CPFCNPJRemetente    := Leitor.rCampo(tcStr, 'CNPJ');
          if (FInfRec.InformacoesLote.CPFCNPJRemetente = '') then
            FInfRec.InformacoesLote.CPFCNPJRemetente  := Leitor.rCampo(tcStr, 'CPF');

          FInfRec.InformacoesLote.DataEnvioLote       := Leitor.rCampo(tcDatHor, 'DataEnvioLote');
          FInfRec.InformacoesLote.QtdNotasProcessadas := Leitor.rCampo(tcInt, 'QtdNotasProcessadas');
          FInfRec.InformacoesLote.TempoProcessamento  := Leitor.rCampo(tcInt, 'TempoProcessamento');
          FInfRec.InformacoesLote.ValorTotalServico   := Leitor.rCampo(tcDe2, 'ValorTotalServicos');
        end;
      end;

      i := 0;
      while Leitor.rExtrai(2, 'ChaveNFeRPS', '', i + 1) <> '' do
      begin
        FInfRec.FListaChaveNFeRPS.New;

        if (leitor.rExtrai(3, 'ChaveNFe') <> '') then
        begin
          FInfRec.ListaChaveNFeRPS[i].ChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfRec.ListaChaveNFeRPS[i].ChaveNFeRPS.Numero := Leitor.rCampo(tcStr, 'NumeroNFe');
          FInfRec.ListaChaveNFeRPS[i].ChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
        end;

        if (leitor.rExtrai(3, 'ChaveRPS') <> '') then
        begin
          FInfRec.ListaChaveNFeRPS[i].ChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfRec.ListaChaveNFeRPS[i].ChaveNFeRPS.SerieRPS := Leitor.rCampo(tcStr, 'SerieRPS');
          FInfRec.ListaChaveNFeRPS[i].ChaveNFeRPS.NumeroRPS := Leitor.rCampo(tcStr, 'NumeroRPS');
        end;

        Inc(i);
      end;

      i := 0;
      while Leitor.rExtrai(2, 'Alerta', '', i + 1) <> '' do
      begin
        FInfRec.MsgRetorno.New;
        FInfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        FInfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Descricao');
        FInfRec.FMsgRetorno[i].FCorrecao := '';

        if (leitor.rExtrai(3, 'ChaveNFe') <> '') then
        begin
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.Numero := Leitor.rCampo(tcStr, 'Numero');
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
        end;

        if (leitor.rExtrai(3, 'ChaveRPS') <> '') then
        begin
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.SerieRPS := Leitor.rCampo(tcStr, 'SerieRPS');
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.NumeroRPS := Leitor.rCampo(tcStr, 'NumeroRPS');
        end;

        Inc(i);
      end;

      i := 0;
      while Leitor.rExtrai(2, 'Erro', '', i + 1) <> '' do
      begin
        FInfRec.MsgRetorno.New;
        FInfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        FInfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Descricao');
        FInfRec.FMsgRetorno[i].FCorrecao := '';

        if (leitor.rExtrai(3, 'ChaveNFe') <> '') then
        begin
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.Numero := Leitor.rCampo(tcStr, 'Numero');
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
        end;

        if (leitor.rExtrai(3, 'ChaveRPS') <> '') then
        begin
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.SerieRPS := Leitor.rCampo(tcStr, 'SerieRPS');
          FInfRec.FMsgRetorno[i].FChaveNFeRPS.NumeroRPS := Leitor.rCampo(tcStr, 'NumeroRPS');
        end;

        Inc(i);
      end;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXML_proSmarapd: Boolean;
begin
  try
    if (Leitor.rExtrai(1, 'recibo') <> '') then
    begin
      if Leitor.rCampo(tcStr, 'Erro') <> 'false' then
      begin
        FInfRec.Protocolo        := Leitor.rCampo(tcStr, 'codrecibo');
        FInfRec.DataRecebimento  := Leitor.rCampo(tcDatVcto, 'datahora');
      end;
    end;

    if pos('Erro',leitor.Arquivo) > 0 Then
    begin
      FInfRec.MsgRetorno.New;
      FInfRec.MsgRetorno[0].FCodigo   := '';
      FInfRec.MsgRetorno[0].FMensagem := leitor.Arquivo;
      FInfRec.MsgRetorno[0].FCorrecao := '';
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXML_proIPM: Boolean;
var
  i: Integer;
begin
  try
    Result := False;

    if (Leitor.rExtrai(1, 'retorno') <> '') then
    begin
      if (Leitor.rExtrai(2, 'mensagem') <> '') then
      begin
        if (copy(Leitor.rCampo(tcStr, 'codigo'), 1, 5) <> '00001') then
        begin
          i := 0;
          while Leitor.rExtrai(3, 'codigo', '', i + 1) <> '' do
          begin
            FInfRec.MsgRetorno.New;
            FInfRec.FMsgRetorno[i].FCodigo   := Copy(Leitor.rCampo(tcStr, 'codigo'), 1, 5);
            FInfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'codigo');
            Inc(i);
          end;
        end
        else
        begin
          if (Leitor.rExtrai(1, 'retorno') <> '') then
          begin
            FInfRec.FNumeroLote      := Leitor.rCampo(tcStr, 'numero_nfse');
            FInfRec.FProtocolo       := Leitor.rCampo(tcStr, 'cod_verificador_autenticidade');
            FInfRec.FDataRecebimento := StrToDateDef(VarToStr(Leitor.rCampo(tcStr, 'data_nfse')), 0) +
                                        StrToTimeDef(VarToStr(Leitor.rCampo(tcStr, 'hora_nfse')), 0);

            with FInfRec.FListaChaveNFeRPS.New do
            begin
              ChaveNFeRPS.Numero            := Leitor.rCampo(tcStr, 'numero_nfse');
              ChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'cod_verificador_autenticidade');
              ChaveNFeRPS.SerieRPS          := Leitor.rCampo(tcStr, 'serie_nfse');
              ChaveNFeRPS.NumeroRPS         := Leitor.rCampo(tcStr, 'numero_nfse');
              ChaveNFeRPS.Link              := Leitor.rCampo(tcStr, 'link_nfse');
            end;
            
            Result := True;
          end;
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXML_proGiap: Boolean;
var
  i, j : Smallint;
  s : String;
  sMessage, sValue : TStringList;
begin
  Result   := False;
  sMessage := TStringList.Create;
  sValue   := TStringList.Create;
  try
    if Leitor.rExtrai(1, 'nfeResposta') <> '' then
    begin
      i := 0;

      while Leitor.rExtrai(2, 'notaFiscal', '', i + 1) <> '' do
      begin
        s := Leitor.rExtrai(2, 'notaFiscal', '', i + 1);
        Result := Leitor.rCampo(tcStr, 'statusEmissao') = '200';

        FInfRec.FSucesso         := BoolToStr(Result, True);
        if Result then
        begin
          FInfRec.FDataRecebimento := Now;
          FInfRec.FProtocolo       := Leitor.rCampo(tcStr, 'statusEmissao');

          with FInfRec.ListaChaveNFeRPS.New do
          begin
            ChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'codigoVerificacao');
            ChaveNFeRPS.Numero            := Leitor.rCampo(tcStr, 'numeroNota');
            ChaveNFeRPS.NumeroRPS         := Leitor.rCampo(tcStr, 'numeroRps');
            ChaveNFeRPS.Link              := Leitor.rCampo(tcStr, 'link');
          end;
        end
        else
        begin
          sMessage.Text:= StringReplace(s, '<messages', #13+'<messages', [rfReplaceAll]);
          sMessage.Text:= StringReplace(sMessage.Text, '</notaFiscal>', '', [rfReplaceAll]);
          for j := 0 to sMessage.Count -1 do
          begin
            if pos('messages', sMessage[j]) > 0 then
            begin
              sValue.Text := StringReplace(sMessage[j], '<messages ', '', [rfReplaceAll]);
              sValue.Text := StringReplace(sValue.Text, '/>', '', [rfReplaceAll]);
              sValue.Text := StringReplace(sValue.Text, 'message', #13+'message', [rfReplaceAll]);
              sValue.Text := StringReplace(sValue.Text, '"', '', [rfReplaceAll]);

              s := sValue.Text;

              if sValue.Count > 1 then
              begin
                with FInfRec.MsgRetorno.New do
                begin
                  FCodigo   := sValue.Values['code'];

                  s := sValue.Values['message'];
                  s := Copy(s, 1, LastDelimiter(':', s) - 1);

                  FMensagem := s;

                  s := sValue.Values['message'];
                  Delete(s, 1, LastDelimiter(':', s));
                  FCorrecao := s;
                end;
              end;
            end;
          end;
        end;

        Inc(i);
      end;
    end;
  finally
    FreeAndNil(sMessage);
    FreeAndNil(sValue);
  end;
end;

end.
