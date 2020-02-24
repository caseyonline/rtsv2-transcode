const path = require("path");
const glob = require("glob");
const webpack = require("webpack");
const srcDir = path.resolve(__dirname, "./modern");
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');
const WebpackAutoInject = require('webpack-auto-inject-version');

module.exports = env => {
  if (!env) {
    env = {
      outName: "llnwrtssdk-2.0.0.js",
      buildDir: path.resolve(__dirname, "../priv/www/assets/js"),
    }
  }

  return {
    entry: {
      "modern": path.resolve(srcDir, "SDK.ts")
    },

    context: srcDir,

    output: {
      filename: env.outName,
      path: env.buildDir,
      library: "LimelightSDK",
      libraryTarget: "umd"
    },

    resolve: {
      modules: ["node_modules"],
      extensions: [".js", ".ts", ".tsx"]
    },

    plugins: [
      new webpack.NoEmitOnErrorsPlugin(),
      new WebpackAutoInject({
        SHORT: 'LLNW RTS SDK',
        components: {
          InjectAsComment: true,
          InjectByTag: true
        },
        componentsOptions: {
          InjectAsComment: {
            tag: 'v{version} built {date}',
            dateFormat: 'isoDateTime'
          },
          InjectByTag: {
            dateFormat: 'isoDateTime'
          }
        }
      }),
      (() =>  {
        if (env.target === "debug") {
          return new webpack.SourceMapDevToolPlugin({});
        }
        else {
          return uglify(true);
        }
      })()
    ],

    module: {
      rules: [
        {
          test: /\.ts(x?)$/,
          exclude: [/node_modules/],
          loader: "ts-loader",
          options: {
            transpileOnly: JSON.parse(env.transpileOnly || "false")
          }
        }
      ]
    }
  }
};

function uglify(sourceMap) {
  // Stolen from: https://slack.engineering/keep-webpack-fast-a-field-guide-for-better-build-performance-f56a5995e8f1
  return new UglifyJsPlugin({
    sourceMap: sourceMap,
    uglifyOptions: {
      mangle: true,
      compress: {
        arrows: false,
        booleans: false,
        collapse_vars: false,
        comparisons: false,
        computed_props: false,
        hoist_funs: false,
        hoist_props: false,
        hoist_vars: false,
        if_return: false,
        inline: false,
        join_vars: false,
        keep_infinity: true,
        loops: false,
        negate_iife: false,
        properties: false,
        reduce_funcs: false,
        reduce_vars: false,
        sequences: false,
        side_effects: false,
        switches: false,
        top_retain: false,
        toplevel: false,
        typeofs: false,
        unused: false,

        // Switch off all types of compression except those needed to convince
        // react-devtools that we're using a production build
        conditionals: true,
        dead_code: true,
        evaluate: true,
      }
    }
  });
}
