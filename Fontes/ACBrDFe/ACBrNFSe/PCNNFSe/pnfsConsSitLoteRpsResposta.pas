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

unit pnfsConsSitLoteRpsResposta;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase, ACBrUtil,
  pcnAuxiliar, pcnConversao, pcnLeitor, pnfsConversao, pnfsNFSe;

type

 TMsgRetornoSitCollection = class;
 TMsgRetornoSitCollectionItem = class;

 TInfSit = class(TObject)
  private
    FNumeroLote: String;
    FSituacao: String;
    FSucesso: String;
    FMsgRetorno: TMsgRetornoSitCollection;
    FInformacoesLote: TInformacoesLote;

    procedure SetMsgRetorno(Value: TMsgRetornoSitCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property NumeroLote: String                   read FNumeroLote write FNumeroLote;
    property Situacao: String                     read FSituacao   write FSituacao;
    property Sucesso: String                      read FSucesso    write FSucesso;
    property MsgRetorno: TMsgRetornoSitCollection read FMsgRetorno write SetMsgRetorno;
    property InformacoesLote: TInformacoesLote    read FInformacoesLote write FInformacoesLote;
  end;

 TMsgRetornoSitCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TMsgRetornoSitCollectionItem;
    procedure SetItem(Index: Integer; Value: TMsgRetornoSitCollectionItem);
  public
    function Add: TMsgRetornoSitCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TMsgRetornoSitCollectionItem;
    property Items[Index: Integer]: TMsgRetornoSitCollectionItem read GetItem write SetItem; default;
  end;

 TMsgRetornoSitCollectionItem = class(TObject)
  private
    FCodigo: String;
    FMensagem: String;
    FCorrecao: String;
    FIdentificacaoRps: TMsgRetornoIdentificacaoRps;
    FChaveNFeRPS: TChaveNFeRPS;
    FchvAcessoNFSe: String;
    Fsit: String;
  public
    constructor Create;
    destructor Destroy; override;

    property Codigo: String   read FCodigo   write FCodigo;
    property Mensagem: String read FMensagem write FMensagem;
    property Correcao: String read FCorrecao write FCorrecao;
    property IdentificacaoRps: TMsgRetornoIdentificacaoRps read FIdentificacaoRps write FIdentificacaoRps;
    property ChaveNFeRPS: TChaveNFeRPS read FChaveNFeRPS write FChaveNFeRPS;
    property chvAcessoNFSe: String read FchvAcessoNFSe write FchvAcessoNFSe;
    property sit: String           read Fsit           write Fsit;
  end;

 { TretSitLote }

 TretSitLote = class(TObject)
  private
    FLeitor: TLeitor;
    FInfSit: TInfSit;
    FProvedor: TnfseProvedor;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    function LerXml_ABRASF: Boolean;

    function LerXml_proCONAM: Boolean;
    function LerXML_proEL: Boolean;
    function LerXML_proEquiplano: Boolean;
    function LerXML_proInfisc: Boolean;
    function LerXml_proISSDSF: Boolean;
    function LerXml_proNFSeBrasil: Boolean;
    function LerXml_proSP: Boolean;
    function LerXML_proAssessorPublico: boolean;
    property Leitor: TLeitor         read FLeitor   write FLeitor;
    property InfSit: TInfSit         read FInfSit   write FInfSit;
    property Provedor: TnfseProvedor read FProvedor write FProvedor;
  end;

implementation

{ TInfSit }

constructor TInfSit.Create;
begin
  inherited Create;
  FMsgRetorno      := TMsgRetornoSitCollection.Create;
  FInformacoesLote := TInformacoesLote.Create;
end;

destructor TInfSit.Destroy;
begin
  FMsgRetorno.Free;
  FInformacoesLote.Free;
  inherited;
end;

procedure TInfSit.SetMsgRetorno(Value: TMsgRetornoSitCollection);
begin
  FMsgRetorno.Assign(Value);
end;

{ TMsgRetornoSitCollection }

function TMsgRetornoSitCollection.Add: TMsgRetornoSitCollectionItem;
begin
  Result := Self.New;
end;

function TMsgRetornoSitCollection.GetItem(
  Index: Integer): TMsgRetornoSitCollectionItem;
begin
  Result := TMsgRetornoSitCollectionItem(inherited Items[Index]);
end;

procedure TMsgRetornoSitCollection.SetItem(Index: Integer;
  Value: TMsgRetornoSitCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TMsgRetornoSitCollection.New: TMsgRetornoSitCollectionItem;
begin
  Result := TMsgRetornoSitCollectionItem.Create;
  Self.Add(Result);
end;

{ TMsgRetornoSitCollectionItem }

constructor TMsgRetornoSitCollectionItem.Create;
begin
  inherited Create;
  FIdentificacaoRps      := TMsgRetornoIdentificacaoRps.Create;
  FIdentificacaoRps.Tipo := trRPS;
  FChaveNFeRPS           := TChaveNFeRPS.Create;
end;

destructor TMsgRetornoSitCollectionItem.Destroy;
begin
  FIdentificacaoRps.Free;
  FChaveNFeRPS.Free;
  inherited;
end;

{ TretSitLote }

constructor TretSitLote.Create;
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FInfSit := TInfSit.Create;
end;

destructor TretSitLote.Destroy;
begin
  FLeitor.Free;
  FInfSit.Free;
  inherited;
end;

function TretSitLote.LerXml: Boolean;
begin
  if Provedor = proISSCuritiba then
    Leitor.Arquivo := RemoverNameSpace(Leitor.Arquivo)
  else
    Leitor.Arquivo := RemoverNameSpace(RemoverAtributos(RetirarPrefixos(Leitor.Arquivo, Provedor), Provedor));

  Leitor.Grupo := Leitor.Arquivo;

  case Provedor of
    proCONAM:      Result := LerXml_proCONAM;
    proISSDSF:     Result := LerXml_proISSDSF;
    proEquiplano:  Result := LerXML_proEquiplano;
    proInfisc,
    proInfiscv11:  Result := LerXml_proInfisc;
    proEL:         Result := LerXML_proEL;
    proNFSeBrasil: Result := LerXml_proNFSeBrasil;
    proSP, 
    proNotaBlu:    Result := LerXml_proSP;
    proAssessorPublico: Result := LerXML_proAssessorPublico;															 
  else
    Result := LerXml_ABRASF;
  end;
end;

function TretSitLote.LerXml_ABRASF: Boolean;
var
  i: Integer;
  ok: Boolean;
begin
  Result := True;

  try
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
          InfSit.FMsgRetorno.New;
          InfSit.FMsgRetorno[i].FIdentificacaoRps.Numero := Leitor.rCampo(tcStr, 'Numero');
          InfSit.FMsgRetorno[i].FIdentificacaoRps.Serie  := Leitor.rCampo(tcStr, 'Serie');
          InfSit.FMsgRetorno[i].FIdentificacaoRps.Tipo   := StrToTipoRPS(ok, Leitor.rCampo(tcStr, 'Tipo'));

          InfSit.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
          InfSit.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Mensagem');
          InfSit.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'Correcao');

          // Provedor Thema: E92 = Lote em Processamento
          if (trim(InfSit.FMsgRetorno[i].FCodigo) <> '') and
             (trim(InfSit.FMsgRetorno[i].FCodigo) <> 'E92') then
            InfSit.FSituacao := 'Erro';

          if trim(InfSit.FMsgRetorno[i].FCodigo) = 'E92' then
            InfSit.FSituacao := '2';

          inc(i);
        end;
      end;
    end;

    i := 0;
    while (Leitor.rExtrai(1, 'Fault', '', i + 1) <> '') do
     begin
       InfSit.FMsgRetorno.New;
       InfSit.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'faultcode');
       InfSit.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'faultstring');
       InfSit.FMsgRetorno[i].FCorrecao := '';

       InfSit.FSituacao := 'Erro';

       inc(i);
     end;
  except
    Result := False;
  end;
end;

function TretSitLote.LerXML_proAssessorPublico: boolean;
begin
  // nada feito aqui
  Result := False;
end;

function TretSitLote.LerXml_proCONAM: Boolean;
var
  sMotCod,sMotDes: String;
  i: Integer;
begin
  try
    if (leitor.rExtrai(1, 'Sdt_consultaprotocoloout') <> '') or
       (leitor.rExtrai(1, 'Sdt_consultanotasprotocoloout') <> '') then
    begin
      FInfSit.FSituacao:= Leitor.rCampo(tcStr, 'PrtXSts'); {1 (Aguardando processamento)
                                                            2 (Em Processamento)
                                                            3 (Rejeitado)
                                                            4 (Rejeitado Parcialmente)
                                                            5 (Processado)}
      FInfSit.FSucesso := Leitor.rCampo(tcStr, 'Id');

      if (FInfSit.FSucesso = 'Arquivo Aceito') then
        FInfSit.FNumeroLote := Leitor.rCampo(tcStr, 'Protocolo');
      if FInfSit.FSituacao <> '5' then
      begin
        if leitor.rExtrai(2, 'Messages') <> '' then
        begin
          i := 0;
          while Leitor.rExtrai(3, 'Message', '', i + 1) <> '' do
          begin
            sMotCod:=Leitor.rCampo(tcStr, 'Id');
            sMotDes:=Leitor.rCampo(tcStr, 'Description');

            InfSit.MsgRetorno.New;
            InfSit.MsgRetorno[i].FCodigo   := sMotCod;
            InfSit.MsgRetorno[i].FMensagem := sMotDes;
            Inc(i);
          end;
        end;
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TretSitLote.LerXml_proISSDSF: Boolean;
begin
  Result := False;
end;

function TretSitLote.LerXML_proEquiplano: Boolean;
var
  i: Integer;
begin
  try
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
          InfSit.FMsgRetorno.New;
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
          InfSit.FMsgRetorno.New;
          InfSit.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'cdMensagem');
          InfSit.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'dsMensagem');
          InfSit.FMsgRetorno[i].FCorrecao := Leitor.rCampo(tcStr, 'dsCorrecao');
          inc(i);
        end;
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TretSitLote.LerXML_proInfisc: Boolean;
var
  i, j: Integer;
  sMotCod, sMotDes, chave, situacao: String;
begin
  try
    InfSit.FNumeroLote := Leitor.rCampo(tcStr, 'cLote');
    situacao := Leitor.rCampo(tcStr, 'sit');
    InfSit.FSituacao   := situacao;

    if InfSit.FSituacao = '217' then // 217 = Fila para processamento
      InfSit.FSituacao := '1'; // 1 = Aguardando processamento

    if InfSit.FSituacao = '100' then
    begin
      InfSit.FSituacao := '4'; // 4 = Processado com Sucesso

      i := 0;
      while Leitor.rExtrai(1, 'NFSe', '', i + 1) <> '' do
      begin
        chave := Leitor.rCampo(tcStr, 'chvAcessoNFSe');

        if Leitor.rCampo(tcStr, 'sit') <> '' then
        begin
          InfSit.FSituacao := Leitor.rCampo(tcStr, 'sit');

          if InfSit.FSituacao = '100' then
            InfSit.FSituacao := '4'
          else if InfSit.FSituacao = '217' then
            InfSit.FSituacao := '2';
        end;

        if (leitor.rExtrai(1, 'motivos') <> '') then
        begin
          j := 0;
          while Leitor.rExtrai(2, 'mot', '', j) <> '' do
          begin
            sMotDes := Leitor.rCampo(tcStr, 'mot');

            if Pos('Error', sMotDes) > 0 then
              sMotCod := OnlyNumber(copy(sMotDes, 1, Pos(' ', sMotDes)))
            else
              sMotCod := '';

            InfSit.FMsgRetorno.New;
            InfSit.FMsgRetorno[j].FCodigo        := sMotCod;
            InfSit.FMsgRetorno[j].FMensagem      := sMotDes;
            InfSit.FMsgRetorno[j].FCorrecao      := '';
            InfSit.FMsgRetorno[j].FchvAcessoNFSe := chave;
            InfSit.FMsgRetorno[j].Fsit           := situacao;

            inc(j);
          end;
        end;

        inc(i);
      end;
    end;

    if InfSit.FSituacao = '200' then
    begin
      InfSit.FSituacao := '3'; // 3 = Processado com Erro
      sMotDes := Leitor.rCampo(tcStr, 'mot');

      if Pos('Error', sMotDes) > 0 then
        sMotCod := OnlyNumber(copy(sMotDes, 1, Pos(' ', sMotDes)))
      else
        sMotCod := '';

      if sMotDes <> '' then
      begin
        InfSit.FMsgRetorno.New;
        InfSit.FMsgRetorno[InfSit.FMsgRetorno.Count - 1].FCodigo        := sMotCod;
        InfSit.FMsgRetorno[InfSit.FMsgRetorno.Count - 1].FMensagem      := sMotDes;
        InfSit.FMsgRetorno[InfSit.FMsgRetorno.Count - 1].FCorrecao      := '';
        InfSit.FMsgRetorno[InfSit.FMsgRetorno.Count - 1].FchvAcessoNFSe := '';
        InfSit.FMsgRetorno[InfSit.FMsgRetorno.Count - 1].Fsit           := situacao;
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TretSitLote.LerXML_proEL: Boolean;
var
  i: Integer;
  Cod, Msg: String;
  strAux: AnsiString;
begin
  try
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
          InfSit.FMsgRetorno.New;
          InfSit.FMsgRetorno[i].Codigo   := Cod;
          InfSit.FMsgRetorno[i].Mensagem := Msg;
          InfSit.FMsgRetorno[i].Correcao := '';
          Inc(i);
        end
        else
          Break;
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TretSitLote.LerXml_proNFSeBrasil: Boolean;
//var
  //ok, nfseGerada: Boolean;
  //i, Item, posI, count: Integer;
  //VersaoXML: String;
  //strAux,strAux2, strItem: AnsiString;
  //leitorAux, leitorItem:TLeitor;
begin
  Result := False;
  (*
  nfseGerada  := False;
   // Luiz Baião 2014.12.03
  try
    VersaoXML      := '1';

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

function TretSitLote.LerXml_proSP: Boolean;
var
  i: Integer;
begin
  Result := False;

  try
    if leitor.rExtrai(1, 'RetornoInformacoesLote') <> '' then
    begin
      if (leitor.rExtrai(2, 'Cabecalho') <> '') then
      begin
        FInfSit.FSucesso  := Leitor.rCampo(tcStr, 'Sucesso');

        if (FInfSit.FSucesso = 'false') then
          FInfSit.FSituacao := '2'
        else
          FInfSit.FSituacao := '4';

        if (leitor.rExtrai(3, 'InformacoesLote') <> '') then
        begin
          FInfSit.InformacoesLote.NumeroLote          := Leitor.rCampo(tcStr, 'NumeroLote');
          FInfSit.InformacoesLote.InscricaoPrestador  := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfSit.InformacoesLote.CPFCNPJRemetente    := Leitor.rCampo(tcStr, 'CNPJ');
          if FInfSit.InformacoesLote.CPFCNPJRemetente = '' then
            FInfSit.InformacoesLote.CPFCNPJRemetente  := Leitor.rCampo(tcStr, 'CPF');
          FInfSit.InformacoesLote.DataEnvioLote       := Leitor.rCampo(tcDatHor, 'DataEnvioLote');
          FInfSit.InformacoesLote.QtdNotasProcessadas := Leitor.rCampo(tcInt, 'QtdeNotasProcessadas');
          FInfSit.InformacoesLote.TempoProcessamento  := Leitor.rCampo(tcInt, 'TempoProcessamento');
          FInfSit.InformacoesLote.ValorTotalServico   := Leitor.rCampo(tcDe2, 'ValorTotalServicos');
        end;
      end;

      i := 0;
      while Leitor.rExtrai(2, 'Alerta', '', i + 1) <> '' do
      begin
        FInfSit.MsgRetorno.New;
        FInfSit.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        FInfSit.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Descricao');
        FInfSit.FMsgRetorno[i].FCorrecao := '';

        if (leitor.rExtrai(3, 'ChaveNFe') <> '') then
        begin
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.Numero := Leitor.rCampo(tcStr, 'Numero');
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
        end;

        if (leitor.rExtrai(3, 'ChaveRPS') <> '') then
        begin
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.SerieRPS := Leitor.rCampo(tcStr, 'SerieRPS');
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.NumeroRPS := Leitor.rCampo(tcStr, 'NumeroRPS');
        end;

        Inc(i);
      end;

      i := 0;
      while Leitor.rExtrai(2, 'Erro', '', i + 1) <> '' do
      begin
        FInfSit.MsgRetorno.New;
        FInfSit.FMsgRetorno[i].FCodigo   := Leitor.rCampo(tcStr, 'Codigo');
        FInfSit.FMsgRetorno[i].FMensagem := Leitor.rCampo(tcStr, 'Descricao');
        FInfSit.FMsgRetorno[i].FCorrecao := '';

        if (leitor.rExtrai(3, 'ChaveNFe') <> '') then
        begin
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.Numero := Leitor.rCampo(tcStr, 'Numero');
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.CodigoVerificacao := Leitor.rCampo(tcStr, 'CodigoVerificacao');
        end;

        if (leitor.rExtrai(3, 'ChaveRPS') <> '') then
        begin
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.InscricaoPrestador := Leitor.rCampo(tcStr, 'InscricaoPrestador');
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.SerieRPS := Leitor.rCampo(tcStr, 'SerieRPS');
          FInfSit.FMsgRetorno[i].FChaveNFeRPS.NumeroRPS := Leitor.rCampo(tcStr, 'NumeroRPS');
        end;

        Inc(i);
      end;

      Result := True;
    end;
  except
    Result := False;
  end;
end;

end.

