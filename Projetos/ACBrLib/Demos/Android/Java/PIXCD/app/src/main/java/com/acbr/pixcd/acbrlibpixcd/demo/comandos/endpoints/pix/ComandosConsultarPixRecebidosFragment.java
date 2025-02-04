package com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.pix;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ComandosConsultarPixRecebidosFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtDataInicialConsultarPIXRecebidos;
    private EditText txtDataFinalConsultarPIXRecebidos;
    private EditText txtTxIdConsultarPIXRecebidos;
    private EditText txtCPFCNPJConsultarPIXRecebidos;
    private EditText txtPagAtualConsultarPIXRecebidos;
    private EditText txtItensPorPaginaConsultarPIXRecebidos;
    private Button btnConsultarPixRecebidos;
    private EditText txtRespostaConsultarPIXRecebidos;
    private Button btnLimparRespostaConsultarPIXRecebidos;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {

        View view = inflater.inflate(R.layout.fragment_comandos_consultar_pix_recebidos, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtDataInicialConsultarPIXRecebidos = view.findViewById(R.id.txtDataInicialConsultarPIXRecebidos);
        txtDataFinalConsultarPIXRecebidos = view.findViewById(R.id.txtDataFinalConsultarPIXRecebidos);
        txtTxIdConsultarPIXRecebidos = view.findViewById(R.id.txtTxIdConsultarPIXRecebidos);
        txtCPFCNPJConsultarPIXRecebidos = view.findViewById(R.id.txtCPFCNPJConsultarPIXRecebidos);
        txtPagAtualConsultarPIXRecebidos = view.findViewById(R.id.txtPagAtualConsultarPIXRecebidos);
        txtItensPorPaginaConsultarPIXRecebidos = view.findViewById(R.id.txtItensPorPaginaConsultarPIXRecebidos);
        btnConsultarPixRecebidos = view.findViewById(R.id.btnConsultarPixRecebidos);
        txtRespostaConsultarPIXRecebidos = view.findViewById(R.id.txtRespostaConsultarPIXRecebidos);
        btnLimparRespostaConsultarPIXRecebidos = view.findViewById(R.id.btnLimparRespostaConsultarPIXRecebidos);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnConsultarPixRecebidos.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ConsultarPixRecebidos();
            }
        });

        btnLimparRespostaConsultarPIXRecebidos.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaConsultaPixRecebidos();
            }
        });

        return view;
    }

    public void ConsultarPixRecebidos() {
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
        txtRespostaConsultarPIXRecebidos.setText("");
        String result = "";
        String dataInicial = txtDataInicialConsultarPIXRecebidos.getText().toString();
        String dataFinal = txtDataFinalConsultarPIXRecebidos.getText().toString();
        String txId = txtTxIdConsultarPIXRecebidos.getText().toString();
        String cpfCnpj = txtCPFCNPJConsultarPIXRecebidos.getText().toString();
        int pagAtual = Integer.parseInt(txtPagAtualConsultarPIXRecebidos.getText().toString());
        int itensPorPagina = Integer.parseInt(txtItensPorPaginaConsultarPIXRecebidos.getText().toString());
        try {
            Date dateInicial = sdf.parse(dataInicial);
            Date dateFinal = sdf.parse(dataFinal);
            double tDateTimeInicial = ACBrPIXCD.convertDateToTDateTime(dateInicial);
            double tDateTimeFinal = ACBrPIXCD.convertDateToTDateTime(dateFinal);
            result = ACBrPIXCD.ConsultarPixRecebidos(tDateTimeInicial, tDateTimeFinal, txId, cpfCnpj, pagAtual, itensPorPagina);
        } catch (Exception ex) {
            Log.e("Erro ao Consultar PIX Recebidos", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsultarPIXRecebidos.setText(result);
        }
    }

    public void LimparRespostaConsultaPixRecebidos(){
        txtRespostaConsultarPIXRecebidos.setText("");
    }
}