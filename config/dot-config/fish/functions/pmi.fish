function pmi --description "Run Pulumi commands against a specific stack file"
    # Usage: pmi [PATH_TO_PULUMI_STACK_FILE] [PULUMI_COMMANDS]...
    set -l stack_file $argv[1]
    set -l rest $argv[2..]

    set -l pulumi_dir (dirname $stack_file)
    set -l stack_name (basename $stack_file | sed 's/Pulumi\.//' | sed 's/\.yaml//')

    echo "pulumi -C $pulumi_dir -s $stack_name $rest"
    pulumi -C $pulumi_dir -s $stack_name $rest
end
