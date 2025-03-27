import argparse
import nbformat
import subprocess


def add_solution_tag(notebook_path, solution_marker, solution_tag='remove_cell'):
    """
    Open the notebook, and for every code cell that contains the solution_marker,
    add the solution_tag to the cell's metadata.
    """
    with open(notebook_path, 'r', encoding='utf-8') as f:
        nb = nbformat.read(f, as_version=4)

    changed = False
    for cell in nb.cells:
        if cell.cell_type == 'code' and solution_marker in cell.source:
            tags = cell.metadata.get('tags', [])
            if solution_tag not in tags:
                tags.append(solution_tag)
                cell.metadata['tags'] = tags
                changed = True

    if changed:
        with open(notebook_path, 'w', encoding='utf-8') as f:
            nbformat.write(nb, f)


def export_notebook(notebook_path, remove_solutions):
    """
    Export the notebook to HTML. If remove_solutions is True, then the
    TagRemovePreprocessor is enabled to remove cells tagged with 'remove_cell'.
    """
    # Build the nbconvert command
    cmd = ['jupyter', 'nbconvert', '--to', 'html', notebook_path]
    if remove_solutions:
        cmd.extend([
            '--TagRemovePreprocessor.enabled=True',
            "--TagRemovePreprocessor.remove_cell_tags=['remove_cell']"
        ])

    subprocess.run(cmd, check=True)


def main():
    parser = argparse.ArgumentParser(
        description="Export a Jupyter Notebook to HTML with optional removal of solution cells."
    )
    parser.add_argument(
        'notebook',
        help="Path to the Jupyter Notebook (.ipynb)"
    )
    parser.add_argument(
        '--marker',
        default='# SOLUTION-TAG',
        help="The marker text to identify solution cells (default: '# SOLUTION-TAG')"
    )
    parser.add_argument(
        '--remove',
        action='store_true',
        help="Remove solution cells in the exported HTML (using nbconvert TagRemovePreprocessor)"
    )

    args = parser.parse_args()

    # Add metadata tags to solution cells (this edits the notebook file)
    add_solution_tag(args.notebook, args.marker)

    # Export the notebook to HTML
    export_notebook(args.notebook, args.remove)

    print("Export completed successfully.")


if __name__ == '__main__':
    main()
