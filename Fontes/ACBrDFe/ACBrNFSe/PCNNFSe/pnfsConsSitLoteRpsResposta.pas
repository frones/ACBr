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

unit pnfsConsSitLoteRpsResposta;

interface

uses
  SysUtils, Classes,
  pcnAuxiliar, pcnConversao, pcnLeitor, pnfsConversao, pnfsNFSe;

type

 TMsgRetornoSitCollection = class;
 TMsgRetornoSitCollectionItem = class;

 TInfSit = class(TPersistent)
  private
    FNumeroLote: string;
    FSituacao: string;
    FMsgRetorno : TMsgRetornoSitCollection;
    procedure SetMsgRetorno(Value: TMsgRetornoSitCollection);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property NumeroLote: string                   read FNumeroLote write FNumeroLote;
    property Situacao: string                     read FSituacao   write FSituacao;
    property MsgRetorno: TMsgRetornoSitCollection read FMsgRetorno write SetMsgRetorno;
  end;

 TMsgRetornoSitCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TMsgRetornoSitCollectionItem;
    procedure SetItem(Index: Integer; Value: TMsgRetornoSitCollectionItem);
  public
    constructor Create(AOwner: TInfSit);
    function Add: TMsgRetornoSitCollectionItem;
    property Items[Index: Integer]: TMsgRetornoSitCollectionItem read GetItem write SetItem; default;
  end;

 // Alterado por Nilton Olher - 20/02/2015
 TMsgRetornoSitIdentificacaoRps = class(TPersistent)
  private
    FNumero: string;
    FSerie: string;
    FTipo: TnfseTipoRps;
  published
    property Numero: string read FNumero write FNumero;
    property Serie: string read FSerie write FSerie;
    property Tipo: TnfseTipoRps read FTipo write FTipo;
  end;

 TMsgRetornoSitCollectionItem = class(TCollectionItem)
  private
    FIdentificacaoRps: TMsgRetornoSitIdentificacaoRps;    // Alterado por Nilton Olher - 20/02/2015
    FCodigo : String;
    FMensagem : String;
    FCorrecao : String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property IdentificacaoRps: TMsgRetornoSitIdentificacaoRps  read FIdentificacaoRps write FIdentificacaoRps;     // Alterado por Nilton Olher - 20/02/2015
    property Codigo: string   read FCodigo   write FCodigo;
    property Mensagem: string read FMensagem write FMensagem;
    property Correcao: string read FCorrecao write FCorrecao;
  end;

 TretSitLote = class(TPersistent)
  private
    FLeitor: TLeitor;
    FInfSit: TInfSit;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
    function LerXML_provedorInfisc: Boolean;
    function LerXML_provedorEquiplano: Boolean;
    function LerXML_provedorEL: Boolean;
	  function LerXml_provedorNFSEBrasil: boolean;
    function LerXml_provedorFissLex: boolean;

  published
    property Leitor: TLeitor  read FLeitor   write FLeitor;
    property InfSit: TInfSit  read FInfSit   write FInfSit;
  end;

implementation

{ TInfSit }

constructor TInfSit.Create;
begin
  FMsgRetorno := TMsgRetornoSitCollection.Create(Self);
end;

destructor TInfSit.Destroy;
begin
  FMsgRetorno.Free;

  inherited;
end;

procedure TInfSit.SetMsgRetorno(Value: TMsgRetornoSitCollection);
begin
  FMsgRetorno.Assign(Value);
end;

{ TMsgRetornoSitCollection }

function TMsgRetornoSitCollection.Add: TMsgRetornoSitCollectionItem;
begin
  Result := TMsgRetornoSitCollectionItem(inherited Add);
  Result.create;
end;

constructor TMsgRetornoSitCollection.Create(AOwner: TInfSit);
begin
  inherited Create(TMsgRetornoSitCollectionItem);
end;

function TMsgRetornoSitCollection.GetItem(
  Index: Integer): TMsgRetornoSitCollectionItem;
begin
  Result := TMsgRetornoSitCollectionItem(inherited GetItem(Index));
end;

procedure TMsgRetornoSitCollection.SetItem(Index: Integer;
  Value: TMsgRetornoSitCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TMsgRetornoSitCollectionItem }

constructor TMsgRetornoSitCollectionItem.Create;
begin
  // Alterado por Nilton Olher - 20/02/2015
  FIdentificacaoRps       := TMsgRetornoSitIdentificacaoRps.Create;
  FIdentificacaoRps.FTipo := trRPS;
end;

destructor TMsgRetornoSitCollectionItem.Destroy;
begin
  // Alterado por Nilton Olher - 20/02/2015
  FIdentificacaoRps.Free;
  inherited;
end;

{ TretSitLote }

constructor TretSitLote.Create;
begin
  FLeitor := TLeitor.Create;
  FInfSit := TInfSit.Create;
end;

destructor TretSitLote.Destroy;
begin
  FLeitor.Free;
  FInfSit.Free;
  inherited;
end;

function TretSitLote.LerXml: boolean;
var
  i: Integer;
  ok: Boolean;
begin
  result := True;

  try
    // Incluido por Ricardo Miranda em 14/03/2013
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Arquivo := StringReplace(Leitor.Arquivo, ' xmlns=""', '', [rfReplaceAll]);
    Leitor.Grupo   := Leitor.Arquivo;

    // Alterado por Akai - L. Massao Aihara 31/10/2013
    if (leitor.rExtrai(1, 'ConsultarSituacaoLoteRpsResposta') <> '') or
       (leitor.rExtrai(1, 'Consultarsituacaoloterpsresposta') <> '') or
       (leitor.rExtrai(1, 'ConsultarLoteRpsResposta') <> '') or
       (leitor.rExtrai(1, 'ConsultarSituacaoLoteRpsResult') <> '') then
    begin
      InfSit.FNumeroLote := Leitor.rCampo(tcStr, 'NumeroLote');
      InfSit.FSituacao   := Leitor.rCampo(tcStr, 'Situacao');

      // FSituacao: 1 = Não Recebido
      //            2 = Não Processado
      //            3 = Processado com Erro
      //            4 = Processado com Sucesso

      // Ler a Lista de Mensagens
      if leitor.rExtrai(2, 'ListaMensagemRetorno') <> '' then
      begin
        i := 0;
        while Leitor.rExtrai(3, 'MensagemRetorno', '', i + 1) <> '' do
        begin
          InfSit.FMsgRetorno.Add;
          InfSit.FMsgRetorno[i].FIdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
          InfSit.FMsgRetorno[i].FIdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
          InfSit.FMsgRetorno[i].FIdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));
          
          InfSit.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
          InfSit.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
          InfSit.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

          inc(i);
        end;
      end;

    end;

    i := 0;
    while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
     begin
       InfSit.FMsgRetorno.Add;
       InfSit.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
       InfSit.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
       InfSit.FMsgRetorno[i].FCorrecao := '';

       inc(i);
     end;
  except
    result := False;
  end;
end;

function TretSitLote.LerXML_provedorInfisc: Boolean;
var
  i: Integer;
  sMotCod,sMotDes: string;
begin
  try
    // Incluido por Dalvan em 21/11/2014
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;

    InfSit.FNumeroLote := Leitor.rCampo(tcStr, 'cLote');
    InfSit.FSituacao   := Leitor.rCampo(tcStr, 'sit');
    if InfSit.FSituacao='100' then
    begin
      InfSit.FSituacao:='4'; //4 = Processado com Sucesso
    end
    else if InfSit.FSituacao='200' then
    begin
      InfSit.FSituacao:='3'; //3 = Processado com Erro
      i := 0;
      if (leitor.rExtrai(1, 'motivos') <> '') then
      begin
        while Leitor.rExtrai(2, 'mot', '', i + 1) <> '' do
        begin
          sMotDes:=Leitor.rCampo(tcStr, 'mot');
          if Pos('Error',sMotDes)>0 then
            sMotCod:=SomenteNumeros(copy(sMotDes,1,Pos(' ',sMotDes)))
          else
            sMotCod:='';
          InfSit.FMsgRetorno.Add;
          InfSit.FMsgRetorno[i].FCodigo   := sMotCod;
          InfSit.FMsgRetorno[i].FMensagem := sMotDes;
          InfSit.FMsgRetorno[i].FCorrecao := '';
          inc(i);
        end;
      end;
    end;
    result := True;
  except
    result := False;
  end;
end;

function TretSitLote.LerXML_provedorEquiplano: Boolean;
var
  i: Integer;
begin
  try
    // Incluido por Ricardo Miranda em 14/03/2013
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Arquivo := StringReplace(Leitor.Arquivo, ' xmlns=""', '', [rfReplaceAll]);
    Leitor.Grupo   := Leitor.Arquivo;

    InfSit.FNumeroLote := Leitor.rCampo(tcStr, 'nrLoteRps');
    InfSit.FSituacao   := Leitor.rCampo(tcStr, 'stLote');
		//1 - Aguardando processamento
		//2 - Não Processado, lote com erro
		//3 - Processado com sucesso
		//4 - Processado com avisos

    if leitor.rExtrai(1, 'mensagemRetorno') <> '' then
      begin
        i := 0;
        if (leitor.rExtrai(2, 'listaErros') <> '') then
          begin
            while Leitor.rExtrai(3, 'erro', '', i + 1) <> '' do
              begin
                InfSit.FMsgRetorno.Add;
                InfSit.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'cdMensagem');
                InfSit.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'dsMensagem');
                InfSit.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'dsCorrecao');

                inc(i);
              end;
          end;

        if (leitor.rExtrai(2, 'listaAlertas') <> '') then
          begin
            while Leitor.rExtrai(3, 'alerta', '', i + 1) <> '' do
              begin
                InfSit.FMsgRetorno.Add;
                InfSit.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'cdMensagem');
                InfSit.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'dsMensagem');
                InfSit.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'dsCorrecao');

                inc(i);
              end;
          end;
      end;

    result := True;
  except
    result := False;
  end;
end;

function TretSitLote.LerXml_provedorNFSEBrasil: boolean;
var
  ok, nfseGerada: boolean;
  i, Item, posI, count: Integer;
  VersaoXML: String;
  strAux,strAux2, strItem: AnsiString;
  leitorAux, leitorItem:TLeitor;
begin
  result      := False;
  nfseGerada  := False;
  (*
   // Luiz Baião 2014.12.03
  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    VersaoXML      := '1';
    Leitor.Grupo   := Leitor.Arquivo;

    // <erros> .. </erros>
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
             strItem := Leitor.rCampo(tcStr, 'erro');

             if strItem <> EmptyStr then begin
                InfSit.MsgRetorno.Add;
                InfSit.MsgRetorno.Items[i].Mensagem := strItem;
                inc(i);

                // FSituacao: 1 = Não Recebido  / 2 = Não Processado  / 3 = Processado com Erro  / 4 = Processado com Sucesso
                InfSit.FSituacao   := '3';
             end;

             LeitorAux.free;
             Delete(strAux, PosI, count);
             posI := pos('<erro>', strAux);
        end;
    end;

    // <confirmacoes> .. </confirmacoes>
    strAux := leitor.rExtrai_NFSEBrasil(1, 'confirmacoes');
    if ( strAux <> emptystr) then begin

      // <confirmacao>
      //    <![CDATA[As notas referentes ao lote RPS de protocolo "2878CB0EB0E8689" foram geradas.]]>
      // </confirmacao>

        posI := 1;
        while ( posI > 0 ) do begin

           count := pos('</confirmacao>', strAux)+13;
           LeitorAux := TLeitor.Create;
           leitorAux.Arquivo := copy(strAux, PosI, count);
           leitorAux.Grupo   := leitorAux.Arquivo;
           strItem := leitorAux.rCampo(tcStr, 'confirmacao');

      // não alimentar a vartiável caso o restorno seja positivo  --> As notas referentes ao lote RPS de protocolo "2878CB0EB0E8689" foram geradas.
      // if NFSeRetorno.InfSit.MsgRetorno.Count>0

          if   (Pos(UpperCase('" foram geradas'), UpperCase(strItem) ) <> 0) and (InfSit.MsgRetorno.Count = 0) then begin

              nfseGerada := true;
              InfSit.MsgRetorno.Clear;
              InfSit.FSituacao   := '4';
          end;

           // 'Lote RPS's registrado.'
           if ( (nfseGerada = false) and (Pos( UpperCase('s registrado.'), UpperCase(strItem)) = 0)) then begin
              InfSit.MsgRetorno.Add;
              InfSit.MsgRetorno.Items[i].Mensagem := strItem;
              inc(i);
           end;

           LeitorAux.free;
           Delete(strAux, PosI, count);
           posI := pos('<confirmacao>', strAux);

        end;
    end;
{
    // <notas> <nota>...</nota> </notas>
    strAux := leitor.rExtrai_NFSEBrasil(1, 'notas');
    if ( strAux <> emptystr) then begin

        posI := 1;
        while ( posI > 0 ) do begin

           count := pos('</nota>', strAux) + 7;
           LeitorAux := TLeitor.Create;
           leitorAux.Arquivo := copy(strAux, PosI, count);
           leitorAux.Grupo   := leitorAux.Arquivo;
           strAux2 := leitorAux.rExtrai_NFSEBrasil(1,'nota');
           strAux2 := Leitor.rCampo(tcStr, 'nota');
           strItem  := Leitor.rCampo(tcStr, 'nota');



           if (InfSit.MsgRetorno.Count = 0) then begin
                nfseGerada := true;
                InfSit.MsgRetorno.Clear;
                InfSit.FSituacao   := '4';
           end;

          // FSituacao: 1 = Não Recebido  / 2 = Não Processado  / 3 = Processado com Erro  / 4 = Processado com Sucesso
           if nfseGerada = false then begin
              InfSit.MsgRetorno.Add;
              InfSit.MsgRetorno.Items[i].Mensagem := strItem;
              inc(i);
           end;

           LeitorAux.free;
           Delete(strAux, PosI, count);
           posI := pos('<confirmacao>', strAux);

        end;

    end;

}



    Result := True;
  except
    result := False;
  end;
  *)
end;

function TretSitLote.LerXML_provedorEL: Boolean;
var
  i: Integer;
  Cod, Msg: String;
  strAux: AnsiString;
begin
  try
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Grupo   := Leitor.Arquivo;

    InfSit.FNumeroLote := Leitor.rCampo(tcStr, 'numeroLote');
    InfSit.FSituacao   := Leitor.rCampo(tcStr, 'situacaoLoteRps');

    // FSituacao: 1 = Não Recebido
    //            2 = Não Processado
    //            3 = Processado com Erro
    //            4 = Processado com Sucesso

    if leitor.rExtrai(1, 'mensagens') <> '' then
    begin
      i := 0;
      while Leitor.rExtrai(1, 'mensagens', '', i + 1) <> '' do
      begin
        strAux := Leitor.rCampo(tcStr, 'mensagens');
        Cod    := Copy(strAux, 1, 4);
        Msg    := Copy(strAux, 8, Length(strAux));
        if Trim(Msg) <> '' then
        begin
          InfSit.FMsgRetorno.Add;
          InfSit.FMsgRetorno[i].Codigo   := Cod;
          InfSit.FMsgRetorno[i].Mensagem := Msg;
          Inc(i);
        end else
          Break;
      end;
    end;

    result := True;
  except
    result := False;
  end;
end;

function TretSitLote.LerXml_provedorFissLex: boolean;
var
  i: Integer;
begin
  result := True;

  try
    // Incluido por Ricardo Miranda em 14/03/2013
    Leitor.Arquivo := NotaUtil.RetirarPrefixos(Leitor.Arquivo);
    Leitor.Arquivo := StringReplace(Leitor.Arquivo, ' xmlns=""', '', [rfReplaceAll]);
    Leitor.Grupo   := Leitor.Arquivo;

    // Alterado por Akai - L. Massao Aihara 31/10/2013
    if (leitor.rExtrai(1, 'ConsultarSituacaoLoteRpsResposta') <> '') or
       (leitor.rExtrai(1, 'Consultarsituacaoloterpsresposta') <> '') or
       (leitor.rExtrai(1, 'ConsultarLoteRpsResposta') <> '') or
       (leitor.rExtrai(1, 'ConsultarSituacaoLoteRpsResult') <> '') then
    begin
      InfSit.FNumeroLote := Leitor.rCampo(tcStr, 'NumeroLote');
      InfSit.FSituacao   := Leitor.rCampo(tcStr, 'Situacao');

      // FSituacao: 1 = Não Recebido
      //            2 = Não Processado
      //            3 = Processado com Erro
      //            4 = Processado com Sucesso

      // Ler a Lista de Mensagens
      if leitor.rExtrai(2, 'ListaMensagemRetorno') <> '' then
      begin
        i := 0;
        while Leitor.rExtrai(3, 'MensagemRetorno', '', i + 1) <> '' do
        begin
          InfSit.FMsgRetorno.Add;

        //  InfSit.FMsgRetorno[i].FIdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
        //  InfSit.FMsgRetorno[i].FIdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
        //  InfSit.FMsgRetorno[i].FIdentificacaoRps.Tipo   := Leitor.rCampo(tcStr, 'Tipo');
          InfSit.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
          InfSit.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
          InfSit.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

          inc(i);
        end;
      end;

    end;

    i := 0;
    while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
     begin
       InfSit.FMsgRetorno.Add;
       InfSit.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
       InfSit.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
       InfSit.FMsgRetorno[i].FCorrecao := '';

       inc(i);
     end;
  except
    result := False;
  end;
end;

end.

