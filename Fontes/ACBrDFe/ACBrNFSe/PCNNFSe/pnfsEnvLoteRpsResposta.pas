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
  SysUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnLeitor, pnfsConversao, pnfsNFSe;

type

 TMsgRetornoEnvCollection     = class;
 TMsgRetornoEnvCollectionItem = class;

  TInfRec = class
  private
    FNumeroLote: String;
    FDataRecebimento: TDateTime;
    FProtocolo: String;
    FSucesso: String;
    FMsgRetorno: TMsgRetornoEnvCollection;

    procedure SetMsgRetorno(Value: TMsgRetornoEnvCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property NumeroLote: String                   read FNumeroLote      write FNumeroLote;
    property DataRecebimento: TDateTime           read FDataRecebimento write FDataRecebimento;
    property Protocolo: String                    read FProtocolo       write FProtocolo;
    property Sucesso: String                      read FSucesso         write FSucesso;
    property MsgRetorno: TMsgRetornoEnvCollection read FMsgRetorno      write SetMsgRetorno;
  end;

 TMsgRetornoEnvCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TMsgRetornoEnvCollectionItem;
    procedure SetItem(Index: Integer; Value: TMsgRetornoEnvCollectionItem);
  public
    constructor Create(AOwner: TInfRec);
    function Add: TMsgRetornoEnvCollectionItem;
    property Items[Index: Integer]: TMsgRetornoEnvCollectionItem read GetItem write SetItem; default;
  end;

 // Alterado por Nilton Olher - 20/02/2015
 TMsgRetornoEnvIdentificacaoRps = class(TPersistent)
  private
    FNumero: string;
    FSerie: string;
    FTipo: TnfseTipoRps;
  published
    property Numero: string read FNumero write FNumero;
    property Serie: string read FSerie write FSerie;
    property Tipo: TnfseTipoRps read FTipo write FTipo;
  end;

 TMsgRetornoEnvCollectionItem = class(TCollectionItem)
  private
    FIdentificacaoRps: TMsgRetornoEnvIdentificacaoRps;  // Alterado por Nilton Olher - 20/02/2015
    FCodigo: String;
    FMensagem: String;
    FCorrecao: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property IdentificacaoRps: TMsgRetornoEnvIdentificacaoRps  read FIdentificacaoRps write FIdentificacaoRps;   // Alterado por Nilton Olher - 20/02/2015
    property Codigo: String   read FCodigo   write FCodigo;
    property Mensagem: String read FMensagem write FMensagem;
    property Correcao: String read FCorrecao write FCorrecao;
  end;

  TretEnvLote = class(TPersistent)
  private
    FLeitor: TLeitor;
    FInfRec: TInfRec;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;
    function LerXml_provedorIssDsf: Boolean;
    function LerXml_provedorInfisc: Boolean;
    function LerXML_provedorEquiplano: Boolean;
    function LerXML_provedorEL: Boolean;
	function LerXml_provedorNFSEBrasil: boolean;
	
  published
    property Leitor: TLeitor read FLeitor write FLeitor;
    property InfRec: TInfRec read FInfRec write FInfRec;
  end;

implementation

{ TInfRec }

constructor TInfRec.Create;
begin
  FMsgRetorno := TMsgRetornoEnvCollection.Create(Self);
end;

destructor TInfRec.Destroy;
begin
  FMsgRetorno.Free;

  inherited;
end;

procedure TInfRec.SetMsgRetorno(Value: TMsgRetornoEnvCollection);
begin
  FMsgRetorno.Assign(Value);
end;

{ TMsgRetornoEnvCollection }

function TMsgRetornoEnvCollection.Add: TMsgRetornoEnvCollectionItem;
begin
  Result := TMsgRetornoEnvCollectionItem(inherited Add);
  Result.create;
end;

constructor TMsgRetornoEnvCollection.Create(AOwner: TInfRec);
begin
  inherited Create(TMsgRetornoEnvCollectionItem);
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

{ TMsgRetornoEnvCollectionItem }

constructor TMsgRetornoEnvCollectionItem.Create;
begin
  // Alterado por Nilton Olher - 20/02/2015
  FIdentificacaoRps       := TMsgRetornoEnvIdentificacaoRps.Create;
  FIdentificacaoRps.FTipo := trRPS;
end;

destructor TMsgRetornoEnvCollectionItem.Destroy;
begin
  // Alterado por Nilton Olher - 20/02/2015
  FIdentificacaoRps.Free;
  inherited;
end;

{ TretEnvLote }

constructor TretEnvLote.Create;
begin
  FLeitor := TLeitor.Create;
  FInfRec := TInfRec.Create
end;

destructor TretEnvLote.Destroy;
begin
  FLeitor.Free;
  FinfRec.Free;
  inherited;
end;

function TretEnvLote.LerXml: boolean;
var
  i: Integer;
  iNivel: Integer;
  Ok: Boolean;
begin
  Result := True;

  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;

    infRec.FNumeroLote := Leitor.rCampo(tcStr, 'NumeroLote');
    infRec.FProtocolo  := Leitor.rCampo(tcStr, 'Protocolo');

    // Alguns provedores retornam apenas a data, sem o horário
    if Length(Leitor.rCampo(tcStr, 'DataRecebimento')) > 10
     then infRec.FDataRecebimento := Leitor.rCampo(tcDatHor, 'DataRecebimento')
     else infRec.FDataRecebimento := Leitor.rCampo(tcDat, 'DataRecebimento');

    iNivel := 1;
    if leitor.rExtrai(2, 'ListaMensagemRetorno') <> ''
     then iNivel := 3
     else if leitor.rExtrai(1, 'ListaMensagemRetorno') <> ''
           then iNivel := 2;

    i := 0;
    while Leitor.rExtrai(iNivel, 'MensagemRetorno', '', i + 1) <> '' do
     begin
       InfRec.FMsgRetorno.Add;
       InfRec.FMsgRetorno[i].FIdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
       InfRec.FMsgRetorno[i].FIdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
       InfRec.FMsgRetorno[i].FIdentificacaoRps.Tipo   := StrToTipoRPS(Ok, Leitor.rCampo(tcStr, 'Tipo'));

       InfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
       InfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
       InfRec.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

       inc(i);
     end;

    i := 0;
    while Leitor.rExtrai(iNivel, 'ErroWebServiceResposta', '', i + 1) <> '' do
     begin
       InfRec.FMsgRetorno.Add;
       InfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'CodigoErro');
       InfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'MensagemErro');
       InfRec.FMsgRetorno[i].FCorrecao := '';

       inc(i);
     end;

    i := 0;
    while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
     begin
       InfRec.FMsgRetorno.Add;
       InfRec.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
       InfRec.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
       InfRec.FMsgRetorno[i].FCorrecao := '';

       inc(i);
     end;

           //        if i = 0 then
//          InfRec.FMsgRetorno.Add;
//      end;

  except
    Result := False;
  end;
end;

function TretEnvLote.LerXml_provedorIssDsf: boolean;
var
  i, posI, count: Integer;
  VersaoXML: String;
  strAux: AnsiString;
  leitorAux: TLeitor;
begin
  Result := False;

  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    VersaoXML      := '1';
    Leitor.Grupo   := Leitor.Arquivo;

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

            while ( posI > 0 ) do begin
               count := pos('</Alerta>', strAux) + 7;

               FInfRec.FMsgRetorno.Add;
               inc(i);

               LeitorAux := TLeitor.Create;
               leitorAux.Arquivo := copy(strAux, PosI, count);
               leitorAux.Grupo   := leitorAux.Arquivo;

               FInfRec.FMsgRetorno[i].FCodigo  := leitorAux.rCampo(tcStr, 'Codigo');
               FInfRec.FMsgRetorno[i].Mensagem := leitorAux.rCampo(tcStr, 'Descricao');

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

            while (posI > 0) do begin
               count := pos('</Erro>', strAux) + 6;

               FInfRec.FMsgRetorno.Add;
               inc(i);

               LeitorAux := TLeitor.Create;
               leitorAux.Arquivo := copy(strAux, PosI, count);
               leitorAux.Grupo   := leitorAux.Arquivo;

               FInfRec.FMsgRetorno[i].FCodigo  := leitorAux.rCampo(tcStr, 'Codigo');
               FInfRec.FMsgRetorno[i].Mensagem := leitorAux.rCampo(tcStr, 'Descricao');

               LeitorAux.free;

               Delete(strAux, PosI, count);
               posI := pos('<Erro>', strAux);
            end;
         end;
      end;
      Result := True;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXml_provedorInfisc: boolean;
var
  sMotCod,sMotDes: string;
begin
  Result := False;
  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;
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
        if Pos('Error',sMotDes)>0 then
          sMotCod:=SomenteNumeros(copy(sMotDes,1,Pos(' ',sMotDes)))
        else
          sMotCod:='';
        InfRec.MsgRetorno.Add;
        InfRec.MsgRetorno[0].FCodigo   := sMotCod;
        InfRec.MsgRetorno[0].FMensagem := sMotDes+' '+
                                          'CNPJ '+Leitor.rCampo(tcStr, 'CNPJ')+' '+
                                          'DATA '+Leitor.rCampo(tcStr, 'dhRecbto');
        InfRec.MsgRetorno[0].FCorrecao := '';
      end;
      Result := True;
    end;
  except
    Result := False;
  end;
end;

function TretEnvLote.LerXML_provedorEquiplano: Boolean;
var
  i: Integer;
begin
  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;

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
          InfRec.FMsgRetorno.Add;
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
          InfRec.FMsgRetorno.Add;
          InfRec.FMsgRetorno[i].FCodigo  := Leitor.rCampo(tcStr, 'cdMensagem');
          InfRec.FMsgRetorno[i].FMensagem:= Leitor.rCampo(tcStr, 'dsMensagem');
          InfRec.FMsgRetorno[i].FCorrecao:= Leitor.rCampo(tcStr, 'dsCorrecao');

          inc(i);
        end;
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;


function TretEnvLote.LerXml_provedorNFSEBrasil: boolean;
var
  ok: boolean;
  i, Item, posI, count: Integer;
  VersaoXML: String;
  strAux,strAux2, strItem: AnsiString;
  leitorAux, leitorItem:TLeitor;
begin
  result := False;
   // Luiz Baião 2014.12.01
  (*
  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    VersaoXML      := '1';
    Leitor.Grupo   := Leitor.Arquivo;

    strAux := leitor.rExtrai_NFSEBrasil(1, 'RespostaLoteRps');

    if ( strAux <> emptystr) then  begin
           FInfRec.FSucesso := Leitor.rCampo(tcStr, 'Sucesso');
           if (FInfRec.FSucesso <> emptystr) then  begin
              FInfRec.FNumeroLote :=  Leitor.rCampo(tcStr, 'NumeroLote');
              FInfRec.Protocolo   :=  Leitor.rCampo(tcStr, 'Protocolo');
           end;

       infRec.FProtocolo := Leitor.rCampo(tcStr, 'Protocolo');
    end;

    strAux := leitor.rExtrai_NFSEBrasil(1, 'erros');
    if ( strAux <> emptystr) then begin

        posI := 1;
        i := 0 ;
        while ( posI > 0 ) do begin
             count := pos('</erro>', strAux) + 7;

             LeitorAux := TLeitor.Create;
             leitorAux.Arquivo := copy(strAux, PosI, count);
             leitorAux.Grupo   := leitorAux.Arquivo;
             strAux2 := leitorAux.rExtrai_NFSEBrasil(1,'erro');
             strAux2 := Leitor.rCampo(tcStr, 'erro');
             FInfRec.FMsgRetorno.Add;
             FInfRec.FMsgRetorno.Items[i].Mensagem := Leitor.rCampo(tcStr, 'erro')+#13;
             inc(i);
             LeitorAux.free;
             Delete(strAux, PosI, count);
             posI := pos('<erro>', strAux);
        end;
    end;

    strAux := leitor.rExtrai_NFSEBrasil(1, 'confirmacoes');
    if ( strAux <> emptystr) then begin

        posI := 1;
        // i := 0 ;
        while ( posI > 0 ) do begin

           count := pos('</confirmacao>', strAux) + 7;
           LeitorAux := TLeitor.Create;
           leitorAux.Arquivo := copy(strAux, PosI, count);
           leitorAux.Grupo   := leitorAux.Arquivo;
           strAux2 := leitorAux.rExtrai_NFSEBrasil(1,'confirmacao');
           strAux2 := Leitor.rCampo(tcStr, 'confirmacao');
           FInfRec.FMsgRetorno.Add;
           FInfRec.FMsgRetorno.Items[i].Mensagem := Leitor.rCampo(tcStr, 'confirmacao')+#13;
           inc(i);
           LeitorAux.free;
           Delete(strAux, PosI, count);
           posI := pos('<confirmacao>', strAux);
        end;
    end;

    Result := True;
  except
    result := False;
  end;
  *)
end;

function TretEnvLote.LerXML_provedorEL: Boolean;
var
  i: Integer;
  Cod, Msg: String;
  strAux: AnsiString;
begin
  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;

    infRec.FNumeroLote      := Leitor.rCampo(tcStr, 'numeroLote');
    infRec.FDataRecebimento := Leitor.rCampo(tcDatHor, 'dataRecebimento');
    infRec.FProtocolo       := Leitor.rCampo(tcStr, 'numeroProtocolo');

    if (Leitor.rExtrai(1, 'mensagens') <> '') then
    begin
      i:= 0;
      while Leitor.rExtrai(1, 'mensagens', '', i + 1) <> '' do
      begin
        strAux := Leitor.rCampo(tcStr, 'mensagens');
        Cod    := Copy(strAux, 1, 4);
        Msg    := Copy(strAux, 8, Length(strAux));
        if Trim(Msg) <> '' then begin
          InfRec.FMsgRetorno.Add;
          InfRec.FMsgRetorno[i].Codigo   := Cod;
          InfRec.FMsgRetorno[i].Mensagem := Msg;
          Inc(i);
        end else
          Break;
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

end.

